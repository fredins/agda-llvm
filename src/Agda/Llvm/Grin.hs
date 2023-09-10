{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}


module Agda.Llvm.Grin (module Agda.Llvm.Grin) where

import           Control.Monad                (replicateM)
import Data.List (singleton)
import Data.Function (on)
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Agda.Compiler.Backend        hiding (Prim)
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Internal         (Type)
import           Agda.Syntax.Literal
import           Agda.TypeChecking.Substitute hiding (applySubstTerm)
import           Agda.Utils.Function
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List              
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.Maybe



data GrinDefinition = GrinDefinition
  { gr_name      :: String
  , gr_isMain    :: Bool
  , gr_primitive :: Maybe TPrim
  , gr_arity     :: Int
  , gr_type      :: Maybe Type
  , gr_term      :: Term
  , gr_args      :: [Abs]
  , gr_return    :: Maybe Abs
  }

lensGrTerm :: Lens' GrinDefinition Term
lensGrTerm f def = f def.gr_term <&> \t -> def{gr_term = t}

updateGrTerm :: LensMap GrinDefinition Term
updateGrTerm = over lensGrTerm

setGrTerm :: LensSet GrinDefinition Term
setGrTerm = set lensGrTerm

getShortName :: GrinDefinition -> String
getShortName = List1.last . list1splitOnDots . gr_name


-- TODO
-- • Remove Loc from Store
-- • Combine Update and UpdateOffset
data Term = Bind Term LAlt
          | Case Val Term [CAlt] -- List1?
          | App Val [Val] 
          | Unit Val
          | Error TError
          | Store Loc Val
          | Fetch' (Maybe Tag) Int (Maybe Int)
          | Update (Maybe Tag) Int Val
          | UpdateOffset Int Int Val
            deriving (Show, Eq, Ord)

data Val = ConstantNode Tag Val [Val]
         | VariableNode Int Val [Val]
         | Tag Tag
         | Empty
         | Lit Literal
         | Var Int
         | Def String
         | Prim TPrim
           deriving (Show, Eq, Ord)


pattern Fetch :: Tag -> Int -> Term
pattern Fetch tag n = Fetch' (Just tag) n Nothing

pattern FetchOpaque :: Int -> Term
pattern FetchOpaque n = Fetch' Nothing n Nothing

pattern FetchOpaqueOffset :: Int -> Int -> Term
pattern FetchOpaqueOffset n1 n2 = Fetch' Nothing n1 (Just n2)

pattern FetchOffset :: Tag -> Int -> Int -> Term
pattern FetchOffset tag n1 n2 = Fetch' (Just tag) n1 (Just n2)

pattern UpdateTag :: Tag -> Int -> Val -> Term
pattern UpdateTag tag n v = Update (Just tag) n v

pattern Drop n = App (Def "drop") [Var n]
pattern Dup n = App (Def "dup") [Var n]

store :: MonadFresh Int m => Val -> m Term
store t = (`Store` t) <$> freshLoc

instance Unreachable Term where
  isUnreachable (Error TUnreachable) = True
  isUnreachable _                    = False

unreachable :: Term
unreachable = Error TUnreachable


data Tag = CTag {tCon :: String, tArity :: Int}
         | FTag {tDef :: String, tArity :: Int}
         | PTag {tDef :: String, tArity :: Int, tApplied :: Int}
           deriving (Show, Eq, Ord)

newtype Gid = Gid{unGid :: Int} deriving (Show, Eq, Ord, Enum)


free :: Int -> Term
free = App (Def "free") . singleton . Var

constantCNode :: String -> [Val] -> Val
constantCNode name vs = ConstantNode (CTag name (length vs)) (mkLit 1) vs

cnat :: Val -> Val
cnat = ConstantNode natTag (mkLit 1) . singleton

constantFNode :: String -> [Val] -> Val
constantFNode name vs = ConstantNode (FTag name (length vs)) (mkLit 1) vs

freshAbs :: MonadFresh Int m => m Abs
freshAbs = MkAbs <$> freshGid

freshLoc :: MonadFresh Int m => m Loc
freshLoc = MkLoc <$> freshGid

freshGid :: MonadFresh Int m => m Gid
freshGid = Gid <$> fresh

data LAlt = LAltConstantNode Tag Abs [Abs] Term 
          | LAltVariableNode Abs Abs [Abs] Term 
          | LAltEmpty Term
          | LAltVar Abs Term
            deriving (Show, Eq, Ord)

data CAlt = CAltConstantNode Tag Abs [Abs] Term
          | CAltTag Tag Term
          | CAltLit Literal Term
            deriving (Show, Eq, Ord)



infixr 2 `BindEmpty`, `Bind`
pattern BindEmpty :: Term -> Term -> Term
pattern t1 `BindEmpty` t2 = Bind t1 (LAltEmpty t2)


--  term ; λ alt →
type BindView = (Term, Term -> LAlt)
bindView :: Term -> Maybe BindView
bindView (t `Bind` alt) = Just (t, fst $ splitLalt alt)
bindView _              = Nothing

newtype Abs = MkAbs{unAbs :: Gid} deriving (Show, Eq, Ord)
newtype Loc = MkLoc{unLoc :: Gid} deriving (Show, Eq, Ord, Enum)

intFromLoc :: LensGet Loc Int
intFromLoc = unGid . unLoc

intFromAbs :: LensGet Abs Int
intFromAbs = unGid . unAbs

lensAbs :: Lens' Abs Int
lensAbs f abs = f (unGid $ unAbs abs) <&> \n -> MkAbs (Gid n)

laltVar :: MonadFresh Int m => Term -> m LAlt
laltVar t = (`LAltVar` t) <$> freshAbs

caltConstantNode :: MonadFresh Int m => Tag -> Term -> m CAlt
caltConstantNode tag t = do
  x <- freshAbs
  xs <- replicateM (tagArity tag) freshAbs
  pure $ CAltConstantNode tag x xs t

laltConstantNode :: MonadFresh Int m => Tag -> Term -> m LAlt
laltConstantNode tag t = do
  x  <- freshAbs
  xs <- replicateM (tagArity tag) freshAbs
  pure $ LAltConstantNode tag x xs t

laltVariableNode :: MonadFresh Int m => Int -> Term -> m LAlt
laltVariableNode n t = do
  tag <- freshAbs
  x   <- freshAbs
  xs  <- replicateM n freshAbs
  pure $ LAltVariableNode tag x xs t

laltBody :: LensGet LAlt Term
laltBody = \case
  LAltConstantNode _ _ _ t -> t
  LAltVariableNode _ _ _ t -> t
  LAltVar _ t            -> t
  LAltEmpty t            -> t

splitLalt :: LAlt -> (Term -> LAlt, Term)
splitLalt (LAltVar x t)                 = (LAltVar x, t)
splitLalt (LAltConstantNode tag x xs t) = (LAltConstantNode tag x xs, t)
splitLalt (LAltVariableNode x1 x2 xs t) = (LAltVariableNode x1 x2 xs, t)
splitLalt (LAltEmpty t)                 = (LAltEmpty, t)

splitLaltWithVars :: LAlt -> (Term -> LAlt, Term, [Abs])
splitLaltWithVars (LAltVar x t)                 = (LAltVar x, t, [x])
splitLaltWithVars (LAltConstantNode tag x xs t) = (LAltConstantNode tag x xs, t, x : xs)
splitLaltWithVars (LAltVariableNode x1 x2 xs t) = (LAltVariableNode x1 x2 xs, t, x1 : x2 : xs)
splitLaltWithVars (LAltEmpty t)                 = (LAltEmpty, t, [])

splitCalt :: CAlt -> (Term -> CAlt, Term)
splitCalt (CAltConstantNode tag x xs t) = (CAltConstantNode tag x xs, t)
splitCalt (CAltTag tag t)               = (CAltTag tag, t)
splitCalt (CAltLit lit t)               = (CAltLit lit, t)

splitCaltWithVars :: CAlt -> (Term -> CAlt, Term, [Abs])
splitCaltWithVars (CAltConstantNode tag x xs t) = (CAltConstantNode tag x xs, t, x : xs)
splitCaltWithVars (CAltTag tag t)               = (CAltTag tag, t, [])
splitCaltWithVars (CAltLit lit t)               = (CAltLit lit, t, [])

tagArity :: Tag -> Int
tagArity = \case
  CTag{..} -> tArity
  FTag{..} -> tArity
  PTag{..} -> tArity

-- Note: the bind should make sense. For example, `unreachable ; λ x → ...` is a bad input.
bindEnd :: Term -> LAlt -> Term
bindEnd (t1 `Bind` (splitLalt -> (mkAlt, t2))) alt = t1 `Bind` mkAlt (bindEnd t2 alt)
bindEnd t alt = t `Bind` alt

unBind :: LAlt -> Term -> Maybe Term
unBind alt1 (t `Bind` alt2) | alt1 == alt2 = Just t
unBind alt (t1 `Bind` (splitLalt -> (mkAlt, t2))) = (t1 `Bind`) . mkAlt <$> unBind alt t2
unBind _ _ = Nothing

infixr 2 `bindVar`, `bindVarL`, `bindVarR`, `bindVarM`
       , `bindEmptyL`, `bindEmptyR`, `bindEmptyM`
       , `bindCnat`, `bindCnatL`, `bindCnatR`, `bindCnatM`

bindVar :: MonadFresh Int m => Term -> Term -> m Term
bindVar t1 t2 = Bind t1 <$> laltVar t2

bindVarL :: MonadFresh Int m => m Term -> Term -> m Term
bindVarL t1 t2 = (Bind <$> t1) <*> laltVar t2

bindVarR :: MonadFresh Int m => Term -> m Term -> m Term
bindVarR t1 t2 = Bind t1 <$> (laltVar =<< t2)

bindVarM :: MonadFresh Int m => m Term -> m Term -> m Term
bindVarM t1 t2 = (Bind <$> t1) <*> (laltVar =<< t2)

bindEmptyL :: MonadFresh Int m => m Term -> Term -> m Term
bindEmptyL t1 t2 = (Bind <$> t1) ?? LAltEmpty t2

bindEmptyR :: MonadFresh Int m => Term -> m Term -> m Term
bindEmptyR t1 t2 = Bind t1 . LAltEmpty <$> t2

bindEmptyM :: MonadFresh Int m => m Term -> m Term -> m Term
bindEmptyM t1 t2 = (Bind <$> t1) <*> (LAltEmpty <$> t2)

bindCnat :: MonadFresh Int m => Term -> Term -> m Term
bindCnat t1 t2 = Bind t1 <$> laltConstantNode natTag t2

bindCnatL :: MonadFresh Int m => m Term -> Term -> m Term
bindCnatL t1 t2 = (Bind <$> t1) <*> laltConstantNode natTag t2

bindCnatR :: MonadFresh Int m => Term -> m Term -> m Term
bindCnatR t1 t2 = Bind t1 <$> (laltConstantNode natTag =<< t2)

bindCnatM :: MonadFresh Int m => m Term -> m Term -> m Term
bindCnatM t1 t2 = (Bind <$> t1) <*> (laltConstantNode natTag =<< t2)

mkLoc :: Int -> Loc
mkLoc = MkLoc . Gid

mkAbs :: Int -> Abs
mkAbs = MkAbs . Gid

mkLit :: Integer -> Val
mkLit = Lit . LitNat

natTag :: Tag
natTag = CTag{tCon = "nat" , tArity = 1}

gatherTags :: Term -> Set Tag
gatherTags (Unit (ConstantNode tag _ _)) = Set.singleton tag
gatherTags (Store _ (ConstantNode tag _ _)) = Set.singleton tag
gatherTags (Store _ (Tag tag)) = Set.singleton tag
gatherTags (t1 `Bind` (snd . splitLalt -> t2)) = on (<>) gatherTags t1 t2
gatherTags (Case _ t alts) = gatherTags t <> foldMap (gatherTags . snd . splitCalt) alts
gatherTags _ = mempty

-----------------------------------------------------------------------
-- * Substitute instances
-----------------------------------------------------------------------

-- TODO
instance Subst Term where
  type SubstArg Term = Val
  applySubst = applySubstTerm

applySubstTerm :: Substitution' (SubstArg Term) -> Term -> Term
applySubstTerm IdS term = term
applySubstTerm rho term = case term of
  App v vs      -> App (applySubst rho v) (applySubst rho vs)
  Bind t alt    -> Bind (applySubst rho t) (applySubst rho alt)
  Case v t alts -> Case (applySubst rho v) (applySubst rho t) (applySubst rho alts)
  Unit v -> Unit (applySubst rho v)
  Store loc v -> Store loc (applySubst rho v)
  Fetch' mtag n mn
    | Var n' <- lookupS rho n -> Fetch' mtag n' mn
    | otherwise -> __IMPOSSIBLE__
  Update tag n t
    | Var n' <- lookupS rho n -> Update tag n' (applySubst rho t)
    | otherwise -> error $ "VAD HÄNDER: " ++ show (lookupS rho n)
  Error{} -> term

instance Subst LAlt where
  type SubstArg LAlt = Val
  applySubst = applySubstLAlt

applySubstLAlt :: Substitution' (SubstArg LAlt) -> LAlt -> LAlt
applySubstLAlt IdS alt            = alt
applySubstLAlt rho (LAltVar abs t) = LAltVar abs $ applySubst (liftS 1 rho) t
applySubstLAlt rho (LAltConstantNode tag x xs t) =
  LAltConstantNode tag x xs $ applySubst (liftS (length $ x : xs) rho) t
applySubstLAlt rho (LAltVariableNode x1 x2 xs t) =
  LAltVariableNode x1 x2 xs $ applySubst (liftS (length $ x1 : x2 : xs) rho) t
applySubstLAlt rho (LAltEmpty t) = LAltEmpty $ applySubst rho t

instance Subst CAlt where
  type SubstArg CAlt = Val
  applySubst = applySubstCAlt

applySubstCAlt :: Substitution' (SubstArg CAlt) -> CAlt -> CAlt
applySubstCAlt IdS alt            = alt
applySubstCAlt rho (CAltConstantNode tag x xs t) =
  CAltConstantNode tag x xs $ applySubst (liftS (length $ x : xs) rho) t
applySubstCAlt rho (CAltTag tag t) = CAltTag tag $ applySubst rho t
applySubstCAlt rho (CAltLit lit t) = CAltLit lit $ applySubst rho t


instance DeBruijn Val where
  deBruijnVar = Var
  deBruijnView (Var n) = Just n
  deBruijnView _       = Nothing

instance Subst Val where
  type SubstArg Val = Val
  applySubst = applySubstVal

applySubstVal :: Substitution' Val -> Val -> Val
applySubstVal rho (ConstantNode tag rc vs) = ConstantNode tag (applySubst rho rc) $ map (applySubst rho) vs
applySubstVal rho (VariableNode n rc vs)
  | Var n' <- lookupS rho n  = VariableNode n' (applySubst rho rc) $ map (applySubst rho) vs
  | otherwise = __IMPOSSIBLE__
applySubstVal _   (Tag tag)             = Tag tag
applySubstVal _   Empty                 = Empty
applySubstVal _   (Lit lit)             = Lit lit
applySubstVal rho (Var n)               = lookupS rho n
applySubstVal _   (Def s)               = Def s
applySubstVal _   (Prim prim)           = Prim prim

-----------------------------------------------------------------------
-- * Pretty printing instances
-----------------------------------------------------------------------

instance Pretty GrinDefinition where
  pretty GrinDefinition{..} = vcat
    [ pretty gr_name <+> ret (sep (map pretty gr_args) <+> text "=")
    , nest 2 $ pretty gr_term
    ]
    where
      ret :: Doc -> Doc
      ret doc = ifJust gr_return (\abs -> text ("r" ++ tail (prettyShow abs)) <+> doc) doc


instance Pretty Term where
  pretty (Bind t alt)
    | isComplicated =
      vcat
        [ text "(" <> pretty t
        , text ") ; λ" <+> go alt <+> text "→"
        , pretty $ laltBody alt
        ]
    | otherwise =
      vcat
        [ pretty t <+> text "; λ" <+> go alt <+> text "→"
        , pretty $ laltBody alt
        ]
    where
      go (LAltConstantNode tag x xs _) = pretty tag <+> sep (map pretty $ x : xs)
      go (LAltVariableNode x1 x2 xs _) = sep (map pretty $ x1 : x2 : xs)
      go (LAltVar x _)                 = pretty x
      go (LAltEmpty _)                 = text "()"

      isComplicated = case t of
        Bind{} -> True
        Case{} -> True
        _      -> False

  pretty (Unit v)
        | ConstantNode{} <- v = text "unit" <+> parens (pretty v)
        | VariableNode{} <- v = text "unit" <+> parens (pretty v)
        | otherwise   = text "unit" <+> pretty v

  pretty (Store l v)
    | Var _ <- v = (text "store" <> pretty l) <+> pretty v
    | otherwise  = (text "store" <> pretty l) <+> parens (pretty v)
  pretty (App v vs) = sep $ pretty v : map pretty vs
  pretty (Case n t alts) =
    vcat
      [ text "case" <+> pretty n <+> text "of"
      , nest 2 $ vcat $ applyWhen (not $ isUnreachable t) (`snoc` docDefault) $ map pretty alts ]
    where
    docDefault = 
      sep [ text "_ →" 
          , nest 2 $ pretty t
          ]

  pretty (Fetch' mtag n moffset) = (text "fetch" <> docTag) <+> pretty n <+> docOffset
    where
    docOffset = maybe mempty (brackets . pretty) moffset
    docTag = maybe mempty pretty mtag
    
  pretty (Update Nothing v1 v2)
    | ConstantNode{} <- v2 = text "update" <+> pretty v1 <+> parens (pretty v2)
    | VariableNode{} <- v2 = text "update" <+> pretty v1 <+> parens (pretty v2)
    | otherwise = text "update" <+> pretty v1 <+> pretty v2
  pretty (Update (Just tag) v1 v2)
    | ConstantNode{} <- v2 = (text "update" <> pretty tag) <+> pretty v1 <+> parens (pretty v2)
    | VariableNode{} <- v2 = (text "update" <> pretty tag) <+> pretty v1 <+> parens (pretty v2)
    | otherwise = (text "update" <> pretty tag) <+> pretty v1 <+> pretty v2
  pretty (Error TUnreachable) = text "unreachable"
  pretty (Error (TMeta _)) = __IMPOSSIBLE__
  pretty (UpdateOffset n1 n2 v) = text "update" <+> pretty n1  <+> brackets (pretty n2) <+> pretty v

instance Pretty Val where
  pretty (VariableNode tag rc vs) = sep (pretty tag : pretty rc : map pretty vs)
  pretty (ConstantNode n rc vs)   = sep (pretty n : pretty rc : map pretty vs)
  pretty (Tag tag)             = pretty tag
  pretty Empty                 = text "()"
  pretty (Lit lit)             = text "#" <> pretty lit
  pretty (Var n)               = pretty n
  pretty (Def q)               = pretty q
  pretty (Prim prim)           = text $ show prim -- FIXME

instance Pretty Abs where
  pretty (MkAbs gid) = text "x" <> pretty gid

instance Pretty Loc where
  pretty (MkLoc gid) = text "l" <> pretty gid

instance Pretty Gid where
  pretty (Gid n) = pretty n

instance Pretty LAlt where
  pretty (LAltConstantNode tag x xs t) =
    sep [ pretty tag <+> sep (map pretty (x : xs)) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (LAltVariableNode x1 x2 xs t) =
    sep [ sep (map pretty $ x1 : x2 : xs) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (LAltVar abs t) =
    sep [ pretty abs <+> text "→"
        , nest 2 $ pretty t
        ]
  pretty (LAltEmpty t) =
    sep [ text "()" <+> text "→"
        , nest 2 $ pretty t
        ]

instance Pretty CAlt where
  pretty (CAltConstantNode tag x xs t) =
    sep [ pretty tag <+> sep (map pretty (x : xs)) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (CAltTag tag t) = sep [pretty tag <+> text "→", nest 2 $ pretty t]
  pretty (CAltLit lit t) =
    sep [ pretty lit <+> text "→"
        , nest 2 $ pretty t
        ]



instance Pretty Tag where
  pretty CTag{..} = text "C" <> pretty tCon
  pretty FTag{..} = text "F" <> pretty tDef
  pretty PTag{..} = text ("P" ++ show (tArity - tApplied)) <> pretty tDef

