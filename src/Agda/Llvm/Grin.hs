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
import           Agda.Utils.Function          (applyWhen)
import           Agda.Utils.Impossible        (__IMPOSSIBLE__)
import           Agda.Utils.Lens
import           Agda.Utils.Maybe             (ifJust)



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



-- TODO refactor
-- • Maybe remove Loc from Store
data Term = Bind Term Alt
          | Case Val Term [Alt]
          | App Val [Val] -- List1
          | Unit Val
          | Store Loc Val
          | Fetch Int
          | Update (Maybe Tag) Int Val
          | Error TError
            deriving (Show, Eq)

data Val = ConstantNode Tag [Val]
         | VariableNode Int [Val]
         | Tag Tag
         | Empty
         | Lit Literal
         | Var Int
         | Def String
         | Prim TPrim
           deriving (Show, Eq)

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

-- TODO seperate into CAlt and LAlt
data Alt = AltConstantNode Tag [Abs] Term
         | AltVariableNode Abs [Abs] Term
         | AltLit Literal Term
         | AltVar Abs Term
         | AltEmpty Term
           deriving (Show, Eq)




infixr 0 `BindEmpty`, `Bind`
pattern BindEmpty :: Term -> Term -> Term
pattern t1 `BindEmpty` t2 = Bind t1 (AltEmpty t2)

newtype Abs = MkAbs{unAbs :: Gid} deriving (Show, Eq, Ord)
newtype Loc = MkLoc{unLoc :: Gid} deriving (Show, Eq, Ord, Enum)

-- lensGetLoc :: LensGet Loc Int
intFromLoc :: LensGet Loc Int
intFromLoc = unGid . unLoc

intFromAbs :: LensGet Abs Int
intFromAbs = unGid . unAbs

lensAbs :: Lens' Abs Int
lensAbs f abs = f (unGid $ unAbs abs) <&> \n -> MkAbs (Gid n)

altVar :: MonadFresh Int m => Term -> m Alt
altVar t = (`AltVar` t) <$> freshAbs

altConstantNode :: MonadFresh Int m => Tag -> Term -> m Alt
altConstantNode tag t = do
  abss <- replicateM (tagArity tag) freshAbs
  pure $ AltConstantNode tag abss t

altBody :: Alt -> Term
altBody = \case
  AltConstantNode _ _ t -> t
  AltVariableNode _ _ t -> t
  AltLit _ t            -> t
  AltVar _ t            -> t
  AltEmpty t            -> t

tagArity :: Tag -> Int
tagArity = \case
  CTag{..} -> tArity
  FTag{..} -> tArity
  PTag{..} -> tArity

infixr 0 `bindVar`, `bindVarL`, `bindVarR`, `bindVarM`
       , `bindEmptyL`, `bindEmptyR`, `bindEmptyM`

bindVar :: MonadFresh Int m => Term -> Term -> m Term
bindVar t1 t2 = Bind t1 <$> altVar t2

bindVarL :: MonadFresh Int m => m Term -> Term -> m Term
bindVarL t1 t2 = (Bind <$> t1) <*> altVar t2

bindVarR :: MonadFresh Int m => Term -> m Term -> m Term
bindVarR t1 t2 = Bind t1 <$> (altVar =<< t2)

bindVarM :: MonadFresh Int m => m Term -> m Term -> m Term
bindVarM t1 t2 = (Bind <$> t1) <*> (altVar =<< t2)

bindEmptyL :: MonadFresh Int m => m Term -> Term -> m Term
bindEmptyL t1 t2 = (Bind <$> t1) ?? AltEmpty t2

bindEmptyR :: MonadFresh Int m => Term -> m Term -> m Term
bindEmptyR t1 t2 = Bind t1 . AltEmpty <$> t2

bindEmptyM :: MonadFresh Int m => m Term -> m Term -> m Term
bindEmptyM t1 t2 = (Bind <$> t1) <*> (AltEmpty <$> t2)



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
  Store loc t -> Store loc (applySubst rho t)
  Fetch n
    | Var n' <- lookupS rho n -> Fetch n'
    | otherwise -> __IMPOSSIBLE__
  Update tag n t
    | Var n' <- lookupS rho n -> Update tag n' (applySubst rho t)
    | otherwise -> __IMPOSSIBLE__
  Error{} -> term

instance Subst Alt where
  type SubstArg Alt = Val
  applySubst = applySubstAlt

applySubstAlt :: Substitution' (SubstArg Alt) -> Alt -> Alt
applySubstAlt IdS alt            = alt
applySubstAlt rho (AltVar abs t) = AltVar abs $ applySubst (liftS 1 rho) t
applySubstAlt rho (AltConstantNode tag abss t) =
  AltConstantNode tag abss $ applySubst (liftS (length abss) rho) t
applySubstAlt rho (AltVariableNode abs abss t) =
  AltVariableNode abs abss $ applySubst (liftS (1 + length abss) rho) t
applySubstAlt rho (AltLit lit t) = AltLit lit $ applySubst rho t
applySubstAlt rho (AltEmpty t) = AltEmpty $ applySubst rho t

instance DeBruijn Val where
  deBruijnVar = Var
  deBruijnView (Var n) = Just n
  deBruijnView _       = Nothing

instance Subst Val where
  type SubstArg Val = Val
  applySubst = applySubstVal

applySubstVal :: Substitution' Val -> Val -> Val
applySubstVal rho (ConstantNode tag vs) = ConstantNode tag $ applySubst rho vs
applySubstVal rho (VariableNode n vs)   = VariableNode n $ applySubst rho vs
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
        , pretty $ altBody alt
        ]
    | otherwise =
      vcat
        [ pretty t <+> text "; λ" <+> go alt <+> text "→"
        , pretty $ altBody alt
        ]
    where
      go (AltConstantNode tag abss _) = pretty tag <+> sep (map pretty abss)
      go (AltVariableNode abs abss _) = pretty abs <+> sep (map pretty abss)
      go (AltLit lit _)               = pretty lit
      go (AltVar abs _)               = pretty abs
      go (AltEmpty _)                 = text "()"

      isComplicated = case t of
        Bind{} -> True
        Case{} -> True
        _      -> False


  pretty (Unit v)
        | ConstantNode{} <- v = text "unit" <+> parens (pretty v)
        | VariableNode{} <- v = text "unit" <+> parens (pretty v)
        | otherwise   = text "unit" <+> pretty v

  pretty (Store l v) = (text "store" <> pretty l) <+> parens (pretty v)
  pretty (App v vs) = sep $ pretty v : map pretty vs
  pretty (Case n def alts) =
    vcat
      [ text "case" <+> pretty n <+> text "of"
      , nest 2 $ vcat $
        applyWhen (not $ isUnreachable def)
        (++ [text "_ →" <+> nest 2 (pretty def)]) $ map pretty alts
      ]

  pretty (Fetch v) = text "fetch" <+> pretty v
  pretty (Update Nothing v1 v2) = text "update" <+> pretty v1 <+> pretty v2
  pretty (Update (Just tag) v1 v2) = (text "update" <> pretty tag) <+> pretty v1 <+> pretty v2
  pretty (Error TUnreachable) = text "unreachable"
  pretty (Error (TMeta _)) = __IMPOSSIBLE__

instance Pretty Val where
  pretty (VariableNode tag vs) = sep (pretty tag : map pretty vs)
  pretty (ConstantNode n vs)   = sep (pretty n : map pretty vs)
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

instance Pretty Alt where
  pretty (AltConstantNode tag gids t) =
    sep [ pretty tag <+> sep (map pretty gids) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (AltVariableNode x gids t) =
    sep [ pretty x <+> sep (map pretty gids) <+> text "→"
        ,  nest 2 $ pretty t
        ]
  pretty (AltLit lit t) =
    sep [ pretty lit <+> text "→"
        , nest 2 $ pretty t
        ]
  pretty (AltVar abs t) =
    sep [ pretty abs <+> text "→"
        , nest 2 $ pretty t
        ]
  pretty (AltEmpty t) =
    sep [ text "()" <+> text "→"
        , nest 2 $ pretty t
        ]

instance Pretty Tag where
  pretty CTag{..} = text "C" <> pretty tCon
  pretty FTag{..} = text "F" <> pretty tDef
  pretty PTag{..} = text ("P" ++ show (tArity - tApplied)) <> pretty tDef

