{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Agda.Llvm.Llvm (module Agda.Llvm.Llvm) where

import           Data.List                 (intercalate, intersperse)
import           Data.String               (IsString)

import           Agda.Compiler.Backend     hiding (Name, Prim)
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal
import           Agda.Utils.Impossible     (__IMPOSSIBLE__)
import           Agda.Utils.List
import           Agda.Utils.List1          (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1          as List1
import           Agda.Utils.Monad          (forM')
import           Control.Arrow             (Arrow (first))
import           Control.Monad             (forM)
import           Data.Foldable             (toList)
import           Data.Semigroup            (sconcat)
import Agda.Llvm.Utils (list1splitOnDots)

data Instruction =
    Define CallingConvention Type GlobalId [(Type, LocalId)] (List1 Instruction)
  | Declare Type LocalId [Type]
  | Getelementptr Type (List1 (Type, Val))
  | Extractvalue Type Val Int
  | Insertvalue Type Val Type Val Int
  | Call (Maybe Tail) CallingConvention Type GlobalId [(Type, Val)]
  | Ret Type (Maybe Val)
  | Store Type Val Type LocalId
  | Load Type Type Val
  | SetVar LocalId Instruction
  | Switch Type LocalId LocalId [Alt]
  | Label String (List1 Instruction)
  | Comment String
  | Unreachable
  | Alloca Type -- not used
  | Inttoptr Type Val Type
  | Ptrtoint Type Val Type
  | Br LocalId
  | Phi Type (List1 (Val, LocalId))
  | Add Type Val Val
  | Sub Type Val Val
    deriving Show

data Tail = Musttail | Tail deriving Show

pattern RetVoid = Ret Void Nothing
pattern RetNode v = Ret (Alias "%Node") (Just v)

data Alt = Alt Type Val LocalId deriving Show

add64 :: Val -> Val -> Instruction
add64 = Add I64

sub64 :: Val -> Val -> Instruction
sub64 = Sub I64

alt :: Val -> LocalId -> Alt
alt = Alt I64

newtype GlobalId = MkGlobalId String deriving (Show, Eq, Ord, IsString)

newtype LocalId = MkLocalId String deriving (Show, Eq, Ord, IsString)

surroundWithQuotes :: String -> String
surroundWithQuotes s = '"' : s `snoc` '"'

mkLocalId :: Pretty a => a -> LocalId
mkLocalId = MkLocalId . ('%' :) . prettyShow

mkUnnamed :: (Pretty a, Num a) => a -> LocalId
mkUnnamed = MkLocalId . ("%" ++) . prettyShow

mkGlobalId :: Pretty a => a -> GlobalId
mkGlobalId (prettyShow -> s) =
  MkGlobalId $ case shortName of
    "main"   -> "@main"
    "free"   -> "@free"
    "printf" -> "@printf"
    "malloc" -> "@malloc"
    _        -> '@' : '"' : s `snoc` '"'
  where
  shortName = List1.last (list1splitOnDots s)


mkLit :: Int -> Val
mkLit = Lit . LitNat . toInteger

data Val = LocalId LocalId
         | GlobalId GlobalId
         | Lit Literal
         | Structure (List1 Val)
         | Null
         | Undef
           deriving (Show, Eq)

data Type = Ptr
          | I8
          | I32
          | I64
          | Void
          | Alias LocalId
          | StructureTy (List1 Type)
          | Varargs
            deriving (Show, Eq)

typeOf :: Val -> Type
typeOf LocalId{}      = Ptr
typeOf GlobalId{}     = Ptr
typeOf (Lit LitNat{}) = I64
typeOf Lit{}          = __IMPOSSIBLE__
typeOf (Structure vs) = StructureTy $ List1.map typeOf vs
typeOf Undef          = __IMPOSSIBLE__
typeOf Null           = __IMPOSSIBLE__

size :: Type -> Int
size I8               = 1
size I64              = 8
size I32              = 4
size Ptr              = 8
size (Alias "%Node")  = size nodeTy
-- size (Alias "%HeapNode")  = size heapNodeTy
size (StructureTy ts) = List1.foldr ((+) . size) size ts
size Varargs          = __IMPOSSIBLE__
size Alias{}          = __IMPOSSIBLE__
size Void             = __IMPOSSIBLE__

nodeSize :: Int
nodeSize = size nodeTy

extractvalue :: Val -> Int -> Instruction
extractvalue = Extractvalue nodeTySyn

insertvalue :: Val -> Val -> Int -> Instruction
insertvalue v  = Insertvalue nodeTySyn v I64

getelementptr :: LocalId -> Int -> Instruction
getelementptr ptr offset = 
  Getelementptr nodeTySyn $ (Ptr, LocalId ptr) :| [(I32, mkLit 0), (I64, mkLit offset)]

switch :: LocalId -> LocalId -> [Alt] -> Instruction
switch = Switch I64

inttoptr :: LocalId -> Instruction
inttoptr x = Inttoptr I64 (LocalId x) Ptr

ptrtoint :: LocalId -> Instruction
ptrtoint x = Ptrtoint Ptr (LocalId x) I64

load :: Type -> LocalId -> Instruction
load t = Load t Ptr . LocalId

nodeTySyn :: Type
nodeTySyn  = Alias "%Node"

nodeTy :: Type
nodeTy = StructureTy $ I64 <| I64 <| I64 <| I64 :| []

heapNodeTy :: Type
heapNodeTy = StructureTy $ I64 <| nodeTySyn :| []

store :: Type -> Val -> LocalId -> Instruction
store t v = Store t v Ptr

malloc :: Int -> Instruction
malloc n = Call Nothing Fastcc Ptr "@malloc" [(I64, mkLit n)]

alloca :: Instruction
alloca = Alloca nodeTySyn

phi :: Type -> List1 (LocalId, LocalId) -> Instruction
phi t = Phi t . List1.map (first LocalId)

data CallingConvention = Tailcc | Fastcc deriving Show

-----------------------------------------------------------------------
-- * Pretty printing instances
-----------------------------------------------------------------------

instance Pretty Instruction where
  pretty = \case
    Unreachable -> text "unreachable"
    Comment s -> text ";" <+> text s
    Define cc t n as is -> vcat
      [ text "define" <+> pretty cc <+> pretty t
      , pretty n <> text ("(" ++ render (prettyArgs as) ++ "){")
      , nest 2 $ vcat $ List1.map pretty is
      , text "}"
      ]
    SetVar x i -> pretty x <+> text "=" <+> pretty i
    Getelementptr t is -> sep
      [ text "getelementptr inbounds"
      , text (prettyShow t ++ ",")
      , text $ intercalate ", " $ toList $ List1.map (\(t, v) -> render $ pretty t <+> pretty v) is
      ]
    Label s is -> vcat [nest (-2) $ text (s `snoc` ':'), vcat $ map pretty $ toList is]
    Switch t x l alts ->
      (text "switch" <+> pretty t <+> (pretty x <> text ", label") <+> pretty l <+> text "[")
      <> vcat (map pretty alts) <> text "]"
    RetVoid -> text "ret void"
    Ret t (Just v) -> text "ret" <+> pretty t <+> pretty v
    Ret{} -> __IMPOSSIBLE__
    Call tc cc t v as -> sep
      [ tc' <> text "call"
      , pretty cc
      , pretty t
      , pretty v <> text ("(" ++ render (prettyArgs as) ++ ")")
      ]
      where tc' = text $ maybe "" ((++ " ") . prettyShow) tc
    Load t1 t2 v -> text "load" <+> text (prettyShow t1 ++ ",") <+> pretty t2 <+> pretty v
    Alloca t -> text "alloca" <+> pretty t -- not used
    Declare{} -> undefined
    Store t1 v t2 x -> (text "store" <+> pretty t1 <+> pretty v) <> (text "," <+> pretty t2 <+> pretty x)
    Extractvalue t v n -> (text "extractvalue" <+> pretty t <+> pretty v) <> (text "," <+> pretty n)
    Insertvalue t1 v1 t2 v2 offset -> (text "insertvalue" <+> pretty t1 <+> pretty v1) <> ("," <+> pretty t2 <+> pretty v2) <> ("," <+> pretty offset)
    Br x -> text "br label" <+> pretty x
    Inttoptr t1 v t2 -> text "inttoptr" <+> pretty t1 <+> pretty v <+> text "to" <+> pretty t2
    Ptrtoint t1 v t2 -> text "ptrtoint" <+> pretty t1 <+> pretty v <+> text "to" <+> pretty t2
    Phi t xs -> (text "phi" <+> pretty t) <> vcat xs''
      where
      xs'' = List1.zipWith (<+>) (List1.map text (" " :| repeat ",")) xs'
      xs' :: List1 Doc
      xs' = List1.map (\(v, x) -> text "[" <> pretty v <> text ", " <> pretty x <> text "]") xs
    Add t v1 v2 -> (text "add" <+> pretty t <+> pretty v1) <> (text "," <+> pretty v2)
    Sub t v1 v2 -> (text "sub" <+> pretty t <+> pretty v1) <> (text "," <+> pretty v2)

instance Pretty Tail where
  pretty Musttail = "musttail"
  pretty Tail = "tail"

instance Pretty Alt where
  pretty (Alt t v x) =
    pretty t <+> text (prettyShow v ++ ", label") <+> pretty x

prettyArgs :: (Foldable t, Pretty a) => t (Type, a) -> Doc
prettyArgs as = text $ intercalate ", " (map go $ toList as)
  where
    go (t, x) = render $ pretty t <+> pretty x

instance Pretty Val where
  pretty = \case
    LocalId x       -> pretty x
    GlobalId x       -> pretty x
    Lit lit         -> pretty lit
    Null            -> text "null"
    Structure vs -> text $ "{" ++ intercalate ", " (map prettyShow $ toList vs) ++ "}"
    Undef -> text "undef"

instance Pretty CallingConvention where
  pretty = \case
    Tailcc -> text "tailcc"
    Fastcc -> text "fastcc" 

instance Pretty Type where
  pretty = \case
    Ptr            -> text "ptr"
    I8             -> text "i8"
    I32            -> text "i32"
    I64            -> text "i64"
    Void           -> text "void"
    Alias x        -> pretty x
    StructureTy ts -> text $ "{" ++ intercalate ", " (map prettyShow $ toList ts) ++ "}"
    Varargs        -> text "..."

instance Pretty LocalId where
  pretty (MkLocalId s) = text s

instance Pretty GlobalId where
  pretty (MkGlobalId s) = text s

