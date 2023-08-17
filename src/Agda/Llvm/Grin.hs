{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}


module Agda.Llvm.Grin (module Agda.Llvm.Grin) where

import           Control.Monad                (replicateM)

import           Agda.Compiler.Backend        hiding (Prim)
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Internal         (Type)
import           Agda.Syntax.Literal
import           Agda.TypeChecking.Substitute hiding (applySubstTerm)
import           Agda.Utils.Function
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.Maybe



data GrinDefinition = GrinDefinition
  { gr_name   :: String
  , gr_isMain :: Bool
  , gr_arity  :: Int
  , gr_type   :: Maybe Type
  , gr_term   :: Term
  , gr_args   :: [Abs]
  , gr_return :: Maybe Abs
  }

lensGrTerm :: Lens' GrinDefinition Term
lensGrTerm f def = f def.gr_term <&> \t -> def{gr_term = t}

updateGrTerm :: LensMap GrinDefinition Term
updateGrTerm = over lensGrTerm



-- TODO
-- • Maybe remove Loc from Store
data Term = Bind Term LAlt
          | Case Val Term [CAlt] -- List1
          | App Val [Val] -- List1
          | Unit Val
          | Store Loc Val
          | Fetch Int (Maybe Int)
          | Update (Maybe Tag) Int Val
          | Error TError
            deriving (Show, Eq)

data Val = ConstantNode Tag (List1 Val)
         | VariableNode Int (List1 Val)
         | Tag Tag
         | Empty
         | Lit Literal
         | Var Int
         | Def String
         | Prim TPrim
           deriving (Show, Eq)

pattern FetchNode :: Int -> Term
pattern FetchNode n = Fetch n Nothing
pattern FetchOffset :: Int -> Int -> Term
pattern FetchOffset n1 n2 = Fetch n1 (Just n2)

pattern UpdateTag :: Tag -> Int -> Val -> Term
pattern UpdateTag tag n v = Update (Just tag) n v

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

freshAbs :: MonadFresh Int m => m Abs
freshAbs = MkAbs <$> freshGid

freshLoc :: MonadFresh Int m => m Loc
freshLoc = MkLoc <$> freshGid

freshGid :: MonadFresh Int m => m Gid
freshGid = Gid <$> fresh

data LAlt = LAltConstantNode Tag [Abs] Term
          | LAltVariableNode Abs [Abs] Term
          | LAltEmpty Term
          | LAltVar Abs Term
            deriving (Show, Eq)

data CAlt = CAltConstantNode Tag [Abs] Term
          | CAltTag Tag Term
          | CAltLit Literal Term
            deriving (Show, Eq)



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
  abss <- replicateM (tagArity tag) freshAbs
  pure $ CAltConstantNode tag abss t

laltConstantNode :: MonadFresh Int m => Tag -> Term -> m LAlt
laltConstantNode tag t = do
  abss <- replicateM (tagArity tag) freshAbs
  pure $ LAltConstantNode tag abss t

laltVariableNode :: MonadFresh Int m => Int -> Term -> m LAlt
laltVariableNode n t = do
  tag <- freshAbs
  xs  <- replicateM n freshAbs
  pure $ LAltVariableNode tag xs t

laltBody :: LensGet LAlt Term
laltBody = \case
  LAltConstantNode _ _ t -> t
  LAltVariableNode _ _ t -> t
  LAltVar _ t            -> t
  LAltEmpty t            -> t

splitLalt :: LAlt -> (Term -> LAlt, Term)
splitLalt (LAltVar abs t)               = (LAltVar abs, t)
splitLalt (LAltConstantNode tag abss t) = (LAltConstantNode tag abss, t)
splitLalt (LAltVariableNode n xs t)     = (LAltVariableNode n xs, t)
splitLalt (LAltEmpty t)                 = (LAltEmpty, t)

splitLaltWithAbss :: LAlt -> (Term -> LAlt, Term, [Abs])
splitLaltWithAbss (LAltVar abs t)               = (LAltVar abs, t, [abs])
splitLaltWithAbss (LAltConstantNode tag abss t) = (LAltConstantNode tag abss, t, abss)
splitLaltWithAbss (LAltVariableNode n xs t)     = (LAltVariableNode n xs, t, xs)
splitLaltWithAbss (LAltEmpty t)                 = (LAltEmpty, t, [])

splitCalt :: CAlt -> (Term -> CAlt, Term)
splitCalt (CAltConstantNode tag abss t) = (CAltConstantNode tag abss, t)
splitCalt (CAltTag tag t)               = (CAltTag tag, t)
splitCalt (CAltLit lit t)               = (CAltLit lit, t)

splitCaltAbss :: CAlt -> (Term -> CAlt, Term, [Abs])
splitCaltAbss (CAltConstantNode tag abss t) = (CAltConstantNode tag abss, t, abss)
splitCaltAbss (CAltTag tag t)               = (CAltTag tag, t, [])
splitCaltAbss (CAltLit lit t)               = (CAltLit lit, t, [])


tagArity :: Tag -> Int
tagArity = \case
  CTag{..} -> tArity
  FTag{..} -> tArity
  PTag{..} -> tArity

infixr 2 `bindVar`, `bindVarL`, `bindVarR`, `bindVarM`
       , `bindEmptyL`, `bindEmptyR`, `bindEmptyM`

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

mkLoc :: Int -> Loc
mkLoc = MkLoc . Gid

mkAbs :: Int -> Abs
mkAbs = MkAbs . Gid

mkLit :: Integer -> Val
mkLit = Lit . LitNat

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
  App t ts      -> App (applySubst rho t) (applySubst rho ts)
  Bind t alt    -> Bind (applySubst rho t) (applySubst rho alt)
  Case t1 t2 alts -> Case (applySubst rho t1) (applySubst rho t2) (applySubst rho alts)
  Unit t -> Unit (applySubst rho t)
  Store loc v -> Store loc (applySubst rho v)
  Fetch n mn
    | Var n' <- lookupS rho n -> Fetch n' mn
    | otherwise -> __IMPOSSIBLE__
  Update tag n t
    | Var n' <- lookupS rho n -> Update tag n' (applySubst rho t)
    | otherwise -> __IMPOSSIBLE__
  Error{} -> term

instance Subst LAlt where
  type SubstArg LAlt = Val
  applySubst = applySubstLAlt

applySubstLAlt :: Substitution' (SubstArg LAlt) -> LAlt -> LAlt
applySubstLAlt IdS alt            = alt
applySubstLAlt rho (LAltVar abs t) = LAltVar abs $ applySubst (liftS 1 rho) t
applySubstLAlt rho (LAltConstantNode tag abss t) =
  LAltConstantNode tag abss $ applySubst (liftS (length abss) rho) t
applySubstLAlt rho (LAltVariableNode abs abss t) =
  LAltVariableNode abs abss $ applySubst (liftS (1 + length abss) rho) t
applySubstLAlt rho (LAltEmpty t) = LAltEmpty $ applySubst rho t

instance Subst CAlt where
  type SubstArg CAlt = Val
  applySubst = applySubstCAlt

applySubstCAlt :: Substitution' (SubstArg CAlt) -> CAlt -> CAlt
applySubstCAlt IdS alt            = alt
applySubstCAlt rho (CAltConstantNode tag abss t) =
  CAltConstantNode tag abss $ applySubst (liftS (length abss) rho) t
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
applySubstVal rho (ConstantNode tag vs) = ConstantNode tag $ List1.map (applySubst rho) vs
applySubstVal rho (VariableNode n vs)
  | Var n' <- lookupS rho n  = VariableNode n' $ List1.map (applySubst rho) vs
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
      go (LAltConstantNode tag abss _) = pretty tag <+> sep (map pretty abss)
      go (LAltVariableNode abs abss _) = pretty abs <+> sep (map pretty abss)
      go (LAltVar abs _)               = pretty abs
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
  pretty (Case n def alts) =
    vcat
      [ text "case" <+> pretty n <+> text "of"
      , nest 2 $ vcat $
        applyWhen (not $ isUnreachable def)
        (++ [text "_ →" <+> nest 2 (pretty def)]) $ map pretty alts
      ]
  pretty (Fetch v mn)
    | Just n <- mn = text "fetch" <+> pretty v <+> brackets (pretty n)
    | otherwise = text "fetch" <+> pretty v
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

instance Pretty Val where
  pretty (VariableNode tag vs) = sep (pretty tag <| List1.map pretty vs)
  pretty (ConstantNode n vs)   = sep (pretty n <| List1.map pretty vs)
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
  pretty (LAltConstantNode tag gids t) =
    sep [ pretty tag <+> sep (map pretty gids) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (LAltVariableNode x gids t) =
    sep [ pretty x <+> sep (map pretty gids) <+> text "→"
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
  pretty (CAltConstantNode tag gids t) =
    sep [ pretty tag <+> sep (map pretty gids) <+> text "→"
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

