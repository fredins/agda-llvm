{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Agda.Llvm.Grin where

import           Agda.Compiler.Backend hiding (Prim)
import           Agda.Syntax.Internal  (Type)
import           Agda.Syntax.Literal
import           Agda.Utils.Pretty


data GrinDefinition = GrinDefinition
  { gName     :: String
  , gType     :: Maybe Type
  , gTerm     :: Term
  , gArity    :: Int
  , gTreeless :: Maybe TTerm
  }

data Term = Bind Term Alt
          | Case Int Term [Alt]
          | App Val [Val]
          | Unit Val
          | Store Val
          | Fetch Val
          | Update Val Val
          | Error TError
            deriving Show

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

data Tag = CTag { tTag :: Int, tCon :: String, tArity :: Int }
         | FTag { tTag :: Int, tDef :: String, tArity :: Int }
         | PTag { tTag :: Int, tDef :: String, tArity :: Int, tApplied :: Int}
           deriving Show


data Alt = AltNode Tag Term
         | AltLit Literal Term
         | AltVar Term
         | AltEmpty Term
           deriving Show

altBody :: Alt -> Term
altBody = \case
  AltNode _ t -> t
  AltLit _ t  -> t
  AltVar t    -> t
  AltEmpty t  -> t

tagArity :: Tag -> Int
tagArity = \case
  CTag{..} -> tArity
  FTag{..} -> tArity
  PTag{..} -> tArity

tag :: Tag -> Int
tag CTag{tTag} = tTag
tag FTag{tTag} = tTag
tag PTag{tTag} = tTag

-----------------------------------------------------------------------
-- * Pretty printing instances
-----------------------------------------------------------------------

instance Pretty GrinDefinition where
  pretty GrinDefinition{..} = vcat
    [ pretty gName <+>
        if gArity == 0
        then text "="
        else text ("#" ++ show gArity) <+> text "="
    , nest 2 $ pretty gTerm
    ]

instance Pretty Term where
  pretty (Bind t alt) =
      vcat
        [ pretty t <+> text "; λ" <+> go alt <+> text "→"
        , pretty $ altBody alt
        ]
    where
      go (AltNode tag t)
        | tagArity tag == 0 = pretty tag
        | otherwise         = pretty tag <+> text ("#" ++ show (tagArity tag))

      go (AltLit lit _)  = pretty lit
      go (AltVar _)      = text "#1"
      go (AltEmpty _)    = text "()"

  pretty (Unit v)
        | Node{} <- v = text "unit" <+> parens (pretty v)
        | otherwise   = text "unit" <+> pretty v

  pretty (Store v) = text "store" <+> parens (pretty v)
  pretty (App v vs) = sep $ map pretty (v : vs)
  pretty (Case n def alts) = sep [text "case" <+> text ("@" ++ show n) <+> text "of"
                             , nest 2 $ vcat $ map pretty alts ++
                                 [ text "_ →" <+> pretty def | not $ isUnreachable def]
                             ]
  pretty (Fetch v) = text "fetch" <+> pretty v
  pretty (Update v1 v2) = text "update" <+> pretty v1 <+> pretty v2
  pretty (Error TUnreachable) = text "unreachable"


instance Pretty Alt where
  pretty (AltNode tag t) | tagArity tag == 0 = pretty tag <+>  text "→" <+> pretty t
                         | otherwise         = pretty tag <+> text ("#" ++ show (tagArity tag)) <+> text "→" <+> pretty t
  pretty (AltLit lit t)  = pretty lit <+> text "→" <+> pretty t
  pretty (AltVar t)      = text "#1" <+> text "→" <+> pretty t
  pretty (AltEmpty t)      = text "()" <+> text "→" <+> pretty t


instance Pretty Tag where
  pretty CTag{..} = text ("C" ++ prettyShow tCon)
  pretty FTag{..} = text ("F" ++ prettyShow tDef)
  pretty PTag{..} = text ("P" ++ show (tArity - tApplied) ++ prettyShow tDef)


instance Pretty Val where
  pretty = \case
    Empty       -> text "()"
    Lit lit     -> pretty lit
    Node tag vs -> sep (pretty tag : map pretty vs)
    Var n       -> text ("@" ++ show n)
    Def q       -> pretty q
    Prim prim   -> text $ show prim -- FIXME


