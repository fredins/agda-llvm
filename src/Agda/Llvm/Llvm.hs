{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.Llvm where

import           Data.List                 (intercalate)

import           Agda.Compiler.Backend     hiding (Name)
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal

data Instruction =
    Define CallingConvention Type Var [(Type, String)] [Instruction]
  | Declare Type Var [Type]
  | Getelementptr Type [(Type, Val)]
  -- | Extractvalue Type
  | Call CallingConvention Type Scope Val [(Type, Val)]
  | Ret Type Val
  | Malloc Int
  | Store Type Val Type Val
  | Load Type Type Val
  | Prim TPrim
  | SetVar Var Instruction
  | Switch Type Var Var [Alt]
  | Label String [Instruction]
  | Comment String
    deriving Show

data Alt = Alt Type Val String deriving Show

newtype Var = MkVar String deriving (Show, Eq)

data Val = Var Var
         | Lit Literal
         | Null
           deriving Show

data Scope = Global | Local deriving Show

data Type = Ptr
          | I8
          | I32
          | I64
          | Void
          | Alias Var
          | Structure [Type]
          | Varargs
            deriving (Show, Eq)


data CallingConvention = Tailcc | Fastcc deriving Show


-----------------------------------------------------------------------
-- * Pretty printing instances
-----------------------------------------------------------------------

instance Pretty Instruction where
  pretty = \case
    Comment s -> text ";" <+> text s
    Define cc t n as is -> vcat
      [ text "define" <+> pretty cc <+> pretty t
      , text $ "@" ++ prettyShow n ++ "(" ++ render (prettyArgs as) ++ "){"
      , nest 2 $ vcat $ map pretty is
      , text "}"
      ]
    SetVar x i -> text ("%" ++ prettyShow x) <+> text "=" <+> pretty i
    Getelementptr t is -> sep
      [ text "getelementptr inbounds"
      , text (prettyShow t ++ ",")
      , text $ intercalate ", " $ map (\(t, v) -> render $ pretty t <+> pp v) is
      ]
      where
        pp (Var (MkVar s)) = text $ "%" ++ s
        pp v               = pretty v
    Label s is -> vcat [nest (-2) $ text $ s ++ ":", vcat $ map pretty is]
    Switch t x l alts ->
          text "switch"
      <+> pretty t
      <+> text ("%" ++ prettyShow x ++ ", label")
      <+> text ("%" ++ prettyShow l)
      <+> text "["
      <+> vcat (map pretty alts)
      <+> text "]"

    Ret t (Var v) -> text "ret" <+> pretty t <+> text ("%" ++ prettyShow v)
    Ret t v -> text "ret" <+> pretty t <+> pretty v

    Call cc t s v as -> sep
      [ text "call"
      , pretty cc
      , pretty t
      , text $ prettyShow s ++ prettyShow v ++ "(" ++ render (prettyArgs as) ++ ")"
      ]

    Load t1 t2 v -> text "load" <+> text (prettyShow t1 ++ ",") <+> pretty t2 <+> text ("%" ++ prettyShow v)

    i -> error $ "not impemented: " ++ show i

instance Pretty Alt where
  pretty (Alt t v x) =
    pretty t <+> text (prettyShow v ++ ", label") <+> text ("%" ++ prettyShow x)

prettyArgs :: Pretty a => [(Type, a)] -> Doc
prettyArgs as = text $ intercalate ", " (map go as)
  where
    go (t, x) = render $ pretty t <+> text ("%" ++ prettyShow x)

instance Pretty Val where
  pretty = \case
    Var x   -> pretty x
    Lit lit -> pretty lit
    Null    -> text "null"


instance Pretty CallingConvention where
  pretty = \case
    Tailcc -> text "tailcc"
    Fastcc -> text "fastcc"

instance Pretty Type where
  pretty = \case
    Ptr          -> text "ptr"
    I8           -> text "i8"
    I32          -> text "i32"
    I64          -> text "i64"
    Void         -> text "void"
    Alias x      -> text $ "%" ++ prettyShow x
    Structure ts -> text $ "{" ++ intercalate ", " (map prettyShow ts) ++ "}"
    Varargs      -> text "..."

instance Pretty Scope where
  pretty Local  = text "%"
  pretty Global = text "@"

instance Pretty Var where
  pretty (MkVar s) = text s





