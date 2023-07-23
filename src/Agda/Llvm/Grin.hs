{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.Grin where

import           Agda.Compiler.Backend hiding (Prim)
import           Agda.Llvm.Utils
import           Agda.Syntax.Internal  (Type)
import           Agda.Syntax.Literal
import           Agda.Utils.Function   (applyWhen)
import           Agda.Utils.Impossible (__IMPOSSIBLE__)
import           Agda.Utils.List       (snoc)
import           Agda.Utils.Maybe      (ifJust)
import           Agda.Utils.Pretty
import           Control.Monad         (replicateM)


data GrinDefinition = GrinDefinition
  { gName     :: String
  , gType     :: Maybe Type
  , gTerm     :: Term
  , gArity    :: Int
  , gArgs     :: [Abs]
  , gReturn   :: Maybe Abs
  , gTreeless :: Maybe TTerm
  }

data Term = Bind Term Alt
          | Case Int Term [Alt]
          | App Val [Val]
          | Unit Val
          | Store Loc Val
          | Fetch Val
          | Update Val Val
          | Error TError
            deriving Show

store :: MonadFresh Int m => Val -> m Term
store v = (`Store` v) <$> freshLoc


instance Unreachable Term where
  isUnreachable (Error TUnreachable) = True
  isUnreachable _                    = False

unreachable :: Term
unreachable = Error TUnreachable

data Val = Node Tag [Val]
         | Lit Literal
         | Empty
         | Var Int
         | Def String
         | Prim TPrim
           deriving Show

data Tag = CTag {tTag :: Int, tCon :: String, tArity :: Int}
         | FTag {tTag :: Int, tDef :: String, tArity :: Int}
         | PTag {tTag :: Int, tDef :: String, tArity :: Int, tApplied :: Int, tArgs :: [Gid]}
           deriving (Show, Eq, Ord)

newtype Gid = Gid{unGid :: Int} deriving (Show, Eq, Ord, Enum)

freshAbs :: MonadFresh Int m => m Abs
freshAbs = MkAbs <$> freshGid

freshLoc :: MonadFresh Int m => m Loc
freshLoc = MkLoc <$> freshGid

freshGid :: MonadFresh Int m => m Gid
freshGid = Gid <$> fresh

data Alt = AltNode Tag [Abs] Term
         | AltLit Literal Term
         | AltVar Abs Term
         | AltEmpty Term
           deriving Show

newtype Abs = MkAbs{unAbs :: Gid} deriving (Show, Eq, Ord)
newtype Loc = MkLoc{unLoc :: Gid} deriving (Show, Eq, Ord)

altVar :: MonadFresh Int m => Term -> m Alt
altVar t = (`AltVar` t) <$> freshAbs

altNode :: MonadFresh Int m => Tag -> Term -> m Alt
altNode tag t = do
  abss <- replicateM (tagArity tag) freshAbs
  pure $ AltNode tag abss t

altBody :: Alt -> Term
altBody = \case
  AltNode _ _ t -> t
  AltLit _ t    -> t
  AltVar _ t    -> t
  AltEmpty t    -> t

tagArity :: Tag -> Int
tagArity = \case
  CTag{..} -> tArity
  FTag{..} -> tArity
  PTag{..} -> tArity

tag :: Tag -> Int
tag CTag{tTag} = tTag
tag FTag{tTag} = tTag
tag PTag{tTag} = tTag


infixr 0 `bindVar`, `bindVarL`, `bindVarR`, `bindVarM`
       , `bindEmpty`, `bindEmptyL`, `bindEmptyR`, `bindEmptyM`

bindVar :: MonadFresh Int m => Term -> Term -> m Term
bindVar t1 t2 = Bind t1 <$> altVar t2

bindVarL :: MonadFresh Int m => m Term -> Term -> m Term
bindVarL t1 t2 = (Bind <$> t1) <*> altVar t2

bindVarR :: MonadFresh Int m => Term -> m Term -> m Term
bindVarR t1 t2 = Bind t1 <$> (altVar =<< t2)

bindVarM :: MonadFresh Int m => m Term -> m Term -> m Term
bindVarM t1 t2 = (Bind <$> t1) <*> (altVar =<< t2)

bindEmpty :: Term -> Term -> Term
bindEmpty t1 t2 = Bind t1 $ AltEmpty t2

bindEmptyL :: MonadFresh Int m => m Term -> Term -> m Term
bindEmptyL t1 t2 = (Bind <$> t1) ?? AltEmpty t2

bindEmptyR :: MonadFresh Int m => Term -> m Term -> m Term
bindEmptyR t1 t2 = Bind t1 . AltEmpty <$> t2

bindEmptyM :: MonadFresh Int m => m Term -> m Term -> m Term
bindEmptyM t1 t2 = (Bind <$> t1) <*> (AltEmpty <$> t2)

-----------------------------------------------------------------------
-- * Pretty printing instances
-----------------------------------------------------------------------

instance Pretty GrinDefinition where
  pretty GrinDefinition{..} = vcat
    [ pretty gName <+> ret (sep (map pretty gArgs) <+> text "=")
    , nest 2 $ pretty gTerm
    ]
    where
      ret :: Doc -> Doc
      ret doc = ifJust gReturn (\abs -> text ("r" ++ tail (prettyShow abs)) <+> doc) doc


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
      go (AltNode tag abss _) = pretty tag <+> sep (map pretty abss)
      go (AltLit lit _)       = pretty lit
      go (AltVar abs _)       = pretty abs
      go (AltEmpty _)         = text "()"

      isComplicated = case t of
        Bind{} -> True
        Case{} -> True
        _      -> False


  pretty (Unit v)
        | Node{} <- v = text "unit" <+> parens (pretty v)
        | otherwise   = text "unit" <+> pretty v

  pretty (Store l v) = (text "store" <> pretty l) <+> parens (pretty v)
  pretty (App v vs) = sep $ map pretty (v : vs)
  pretty (Case n def alts) =
    vcat
      [ text "case" <+> pretty n <+> text "of"
      , nest 2 $ vcat $
        applyWhen (not $ isUnreachable def)
        (++ [text "_ →" <+> nest 2 (pretty def)]) $ map pretty alts
      ]




  -- pretty (Case n def alts) = sep [text "case" <+> pretty n <+> text "of"
  --                            , nest 2 $ vcat $ map pretty alts ++
  --                                [ sep [text "_ →", nest 2 $ pretty def] | not $ isUnreachable def]
  --                            ]
  pretty (Fetch v) = text "fetch" <+> pretty v
  pretty (Update v1 v2) = text "update" <+> pretty v1 <+> pretty v2
  pretty (Error TUnreachable) = text "unreachable"
  pretty _ = __IMPOSSIBLE__


instance Pretty Abs where
  pretty (MkAbs gid) = text "x" <> pretty gid

instance Pretty Loc where
  pretty (MkLoc gid) = text "l" <> pretty gid

instance Pretty Gid where
  pretty (Gid n) = pretty n

instance Pretty Alt where
  pretty (AltNode tag gids t) =
    sep [ pretty tag <+> sep (map pretty gids) <+> text "→"
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


instance Pretty Val where
  pretty = \case
    Empty       -> text "()"
    Lit lit     -> text "#" <> pretty lit
    Node tag vs -> sep (pretty tag : map pretty vs)
    Var n       -> pretty n
    Def q       -> pretty q
    Prim prim   -> text $ show prim -- FIXME


