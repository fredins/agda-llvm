{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}

module Compiler.Grin.HeapPointsToType where

import           Control.Monad                      (join)
import           Control.Monad.Reader               (MonadReader (ask, local),
                                                     Reader, ReaderT, asks,
                                                     runReader)
import           Control.Monad.State                (State, evalState, gets,
                                                     modify)
import           Data.Bifunctor                     (Bifunctor (bimap))
import           Data.Foldable                      (fold, foldrM, toList)
import           Data.Function                      (on)
import           Data.List                          (find, insert, intercalate,
                                                     intersectBy, nub,
                                                     partition, sortOn, (\\))
import           Data.List.Extra                    (firstJust)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Tuple.Extra                   (both)
import           Prelude                            hiding ((!!))

import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.SizedTypes.Utils (trace)
import           Agda.Utils.Function                (applyWhen)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1                   (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1                   as List1
import           Agda.Utils.Maybe
import           GHC.IO                             (unsafePerformIO)

import           Compiler.Grin.Grin                 hiding (cnat)
import           Utils.Utils



data Value = VNode Tag [Value]
           | Bas
           | Pick Value Tag Int
           | EVAL Value
           | FETCH Value
           | Union Value Value
           | Loc Loc
           | Abs Abs
             deriving (Eq, Ord)

valueToList :: Value -> List1 Value
valueToList (Union a b) = on (<>) valueToList a b
valueToList a           = a :| []

listToValue :: List1 Value -> Value
listToValue = foldr1 mkUnion

instance Pretty Value where
  pretty (VNode tag vs) =
    pretty tag <+> text ("[" ++ intercalate ", " (map prettyShow vs) ++ "]")
  pretty Bas            = text "BAS"
  pretty (Pick v tag i) = pretty v <+> text "↓" <+> pretty tag <+> text "↓" <+> pretty i
  pretty (EVAL v) = text "EVAL(" <> pretty v <> text ")"
  pretty (FETCH v) = text "FETCH(" <> pretty v <> ")"
  pretty (Union v1 v2) = pretty v1 <+> text "∪" <+> pretty v2
  pretty (Abs abs) = pretty abs
  pretty (Loc loc) = pretty loc

vnodeView :: Value -> Maybe (Tag, [Value])
vnodeView (VNode tag vs) = Just (tag, vs)
vnodeView _              = Nothing

mkUnion :: Value -> Value -> Value
mkUnion a b
  | a == b = a
  | otherwise = Union a b


data AbstractContext = AbstractContext
  { fHeap :: AbsHeap
  , fEnv  :: AbsEnv
  } deriving Eq

lensAbsHeap :: Lens' AbstractContext [(Loc, Value)]
lensAbsHeap f absCxt = f (unAbsHeap absCxt.fHeap) <&> \heap -> absCxt{fHeap = AbsHeap heap}

lensUnAbsHeap :: Lens' AbsHeap [(Loc, Value)]
lensUnAbsHeap f absHeap = AbsHeap <$> f absHeap.unAbsHeap

lensAbsEnv :: Lens' AbstractContext [(Abs, Value)]
lensAbsEnv f absCxt = f (unAbsEnv absCxt.fEnv) <&> \env -> absCxt{fEnv = AbsEnv env}

instance Pretty AbstractContext where
  pretty (AbstractContext {fHeap, fEnv}) =
    vcat
      [ text "Abstract Heap:"
      , pretty fHeap
      , text "\nAbstract Enviroment:"
      , pretty fEnv
      ]

instance Semigroup AbstractContext where
  cxt1 <> cxt2 = AbstractContext
    { fHeap = AbsHeap $ on unionNub (unAbsHeap . fHeap) cxt1 cxt2
    , fEnv = AbsEnv $ on unionNub (unAbsEnv . fEnv) cxt1 cxt2
    }

instance Monoid AbstractContext where
  mempty = AbstractContext{fHeap = AbsHeap [], fEnv = AbsEnv []}


newtype AbsHeap = AbsHeap{unAbsHeap :: [(Loc, Value)]} deriving Eq

instance Semigroup AbsHeap where
  (<>) = composeAbsHeap

composeAbsHeap :: AbsHeap -> AbsHeap -> AbsHeap
composeAbsHeap (AbsHeap h1) (AbsHeap h2) =
    AbsHeap $ (h1 \\ common) ++ common' ++ (h2 \\ common)
  where
    common = let insec = intersectBy (on (==) fst) in insec h1 h2 ++ insec h2 h1
    locs = nub $ map fst common
    common'
      | null common = []
      | otherwise =
        for locs $ \loc ->
          let vs = mapMaybe (\(loc', v) -> boolToMaybe (loc'==loc) v) common in
          (loc, foldl1 mkUnion vs)

instance Semigroup AbsEnv where
  (<>) = composeAbsEnv

composeAbsEnv :: AbsEnv -> AbsEnv -> AbsEnv
composeAbsEnv (AbsEnv e1) (AbsEnv e2) =
    AbsEnv $ (e1 \\ common) ++ common' ++ (e2 \\ common)
  where
    common = let insec = intersectBy (on (==) fst) in insec e1 e2 ++ insec e2 e1
    abss = nub $ map fst common
    common'
      | null common = []
      | otherwise =
        for abss $ \abs ->
          let vs = mapMaybe (\(abs', v) -> boolToMaybe (abs'==abs) v) common in
          (abs, foldl1 mkUnion vs)

newtype AbsEnv = AbsEnv{unAbsEnv :: [(Abs, Value)]}deriving Eq

instance Pretty AbsHeap where
  pretty (AbsHeap heap) =
      vcat $ map prettyEntry heap
    where
      prettyEntry :: (Loc, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v

instance Pretty AbsEnv where
  pretty (AbsEnv env) =
      vcat $ map prettyEntry env
    where
      prettyEntry :: (Abs, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v
