{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}


module Agda.Llvm.SolveEquations (solveEquations) where

import           Control.Monad.Reader      (MonadReader (ask, local), Reader,
                                            asks, runReader, ReaderT)
import           Control.Monad.State       (State, evalState, gets, modify)
import Control.Monad (join)
import           Data.Foldable             (foldrM, toList, fold)
import Data.Tuple.Extra (both, fst3, snd3)
import           Data.Function             (on)
import           Data.List                 (find, insert, intercalate,
                                            intersectBy, nub, partition, sortOn,
                                            (\\))
import           Data.List.Extra           (firstJust)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Prelude                   hiding ((!!))

import           Agda.Llvm.Grin            hiding (cnat)
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Utils.Function       (applyWhen)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1          as List1
import           Agda.Utils.Maybe


import Data.Graph
import Data.Tree (drawTree)
import Agda.Llvm.HeapPointsToType 
import GHC.IO (unsafePerformIO)

childrenFromValue :: Value -> [Gid]
childrenFromValue (Union v1 v2) = on (++) childrenFromValue v1 v2
childrenFromValue (VNode _ vs) = concatMap childrenFromValue vs
childrenFromValue Bas = []
childrenFromValue (Pick v _ _) = childrenFromValue v
childrenFromValue (EVAL v) = childrenFromValue v
childrenFromValue (FETCH v) = childrenFromValue v
childrenFromValue (Loc loc) = [loc.unLoc]
childrenFromValue (Abs x) = [x.unAbs]

type Node = Either (Loc, Value) (Abs, Value)

edgeListFromContext :: AbstractContext -> [(Node, Gid, [Gid])]
edgeListFromContext (AbstractContext (AbsHeap heap) (AbsEnv env)) =
  heapEdges ++ envEdges
  where
  heapEdges, envEdges :: [(Node , Gid, [Gid])]
  heapEdges = map (\(loc, val) -> (Left (loc, val), loc.unLoc, childrenFromValue val)) heap
  envEdges = map (\(x, val) -> (Right (x, val), x.unAbs, childrenFromValue val)) env

-- Depth-first ordering [Aho el al, 2006]
dfo :: Tree Vertex -> [Vertex]
dfo (Node parent children) = parent : foldr (\tree vs -> vs ++ dfo tree) [] children

-- TODO [Boquist, 1996] recommends the depth-first ordering [Aho, 2006] but this approach doesn't 
--      converge for our test program. Need to look into this further.
solveEquations :: [GrinDefinition] -> AbstractContext -> AbstractContext
solveEquations definitions context = 
  -- logg (
  --   "\nnode order:\n" ++ intercalate "\n" (map (render . nest 4 . pretty) nodeOrder) 
  --   ++ 
  --   "\ngid order:\n" ++ intercalate "\n" (map (render . nest 4 . pretty) gidOrder)) $
  fix definitions newContext nodeOrder
  where
  (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ edgeListFromContext context
  forest = dff graph

  
  -- f = listCase Nothing (List1.map (fst3 . nodeFromVertex) .: (:|)) . dfo

  nodeOrder :: [List1 Node]
  nodeOrder = mapMaybe (listCase Nothing (Just . List1.map (fst3 . nodeFromVertex) .: (:|)) . dfo) forest
  -- nodeOrder = mapMaybe (listCase Nothing (List1.map (fst3 . nodeFromVertex) .: (:|)) . dfo) forest
  gidOrder = map (map (snd3 . nodeFromVertex) . dfo) forest

  newContext = AbstractContext (AbsHeap []) (AbsEnv [])
  

logg s x = unsafePerformIO $ do
  putStrLn s
  pure x


simplifyHeap :: [GrinDefinition] -> AbstractContext -> AbstractContext
simplifyHeap defs context = foldr step context $ unAbsHeap context.fHeap
  where
  step :: (Loc, Value) -> AbstractContext -> AbstractContext
  step (loc, value) context = locHeapUpdate loc (simplify defs context value) context

simplifyEnv :: [GrinDefinition] -> AbstractContext -> AbstractContext
simplifyEnv defs context = foldr step context $ unAbsEnv context.fEnv
  where
  step :: (Abs, Value) -> AbstractContext -> AbstractContext
  step (x, value) context = absEnvUpdate x (simplify defs context value) context

fixCurrent :: [GrinDefinition] -> AbstractContext -> AbstractContext
fixCurrent defs cxt
  | cxt == cxt' = logg ("--SAME-------------------\n" ++ prettyShow cxt) cxt'
  | otherwise = fixCurrent defs $ logg ("--FIX--------------------\n" ++ prettyShow cxt) cxt'
  where
  cxt' = simplifyEnv defs $ simplifyHeap defs cxt

fix :: [GrinDefinition] -> AbstractContext -> [List1 Node] -> AbstractContext
fix defs cxt missingNodes
  | cxt == cxt'' = cxt''
  | otherwise   = fix defs cxt'' missingNodes'
  where
  (cxt', missingNodes') = case missingNodes of
    [] -> (cxt,  [])
    (Left entry :| xs) : ys -> 
      (heapInsert entry cxt, caseList xs ys $ \ x xs -> (x :| xs) : ys)
    (Right entry :| xs) : ys -> 
      (envInsert entry cxt, caseList xs ys $ \ x xs -> (x :| xs) : ys)
  
  cxt'' = fixCurrent defs cxt'



-- TODO fix __IMPOSSIBLE__ cases
simplify :: [GrinDefinition] -> AbstractContext -> Value -> Value
simplify defs cxt@AbstractContext{fHeap, fEnv} = go 
  where
  envLookup :: Abs -> Maybe Value
  envLookup abs = lookup abs $ unAbsEnv fEnv

  heapLookup :: Loc -> Maybe Value
  heapLookup loc = lookup loc $ unAbsHeap fHeap

  defReturnLookup :: String -> Maybe Abs
  defReturnLookup name =
    firstJust (\def -> boolToMaybe (def.gr_name == name) $ fromMaybe __IMPOSSIBLE__ def.gr_return) defs

  go :: Value -> Value
  go (Loc loc) = Loc loc
  go Bas = Bas
  go (VNode tag vs) = VNode tag $ map go vs

  -- Replace with pointee
  go (Abs abs) = fromMaybe (Abs abs) $ envLookup abs
  go (Union v1 v2)
    -- Filter duplicates
    | v <- listToValue $ List1.nub $ valueToList (Union v1 v2)
    , v /= Union v1 v2 = v

    -- Filter self references
    | (_:_, v3 : vs3) <- List1.partition isSelfReference $ valueToList (Union v1 v2) = listToValue (v3 :| vs3)

    -- Gather node values of same tag
    -- {... tag [v₁,...,vᵢ,...],...} ∪ {... tag [w₁,...,wᵢ,...],...} =
    -- {... tag [v₁ ∪ w₁,...,vᵢ ∪ wᵢ,...],...}
    | (node : nodes, vs1) <- mapMaybeAndRest vnodeView $ List1.toList $ valueToList (Union v1 v2)
    , nodes' <- List1.groupAllWith1 fst (node :| nodes)
    , any ((> 1) . length) nodes' =
      let vs2 = List1.map (uncurry VNode . foldr1 step) nodes'
          step (tag, vs1) (_, vs2) = (tag, zipWith mkUnion vs1 vs2) in
      listToValue $ vs1 `List1.prependList` vs2

    -- Recurse
    | otherwise = on mkUnion go v1 v2

    where
      -- isSelfReference (Pick v _ _) = isSelfReference v
      -- isSelfReference (EVAL v) = isSelfReference v
      -- isSelfReference (FETCH v) = isSelfReference v
      isSelfReference (Abs (envLookup -> Just v)) = v == Union v1 v2
      isSelfReference _ = False

  go (Pick v1 tag1 i)
    | VNode tag2 vs <- v1
    , tag1 == tag2 = fromMaybe __IMPOSSIBLE__ $ vs !!! i
    | VNode{} <- v1 = __IMPOSSIBLE__

    -- Filter everything which is confirmed wrong
    | Union{} <- v1
    , (_:_, v2 : v2s) <- List1.partition isWrong $ valueToList v1 =
      Pick (listToValue $ v2 :| v2s) tag1 i

    -- Solve correct tags
    | Union{} <- v1
    , (v2 : v2s, v3s) <- mapMaybeAndRest isCorrect $ List1.toList $ valueToList v1 =
      caseList v3s
        (listToValue $ v2 :| v2s)
        (\v3 v3s ->
          listToValue (v2 :| v2s) `mkUnion` Pick (listToValue $ v3 :| v3s) tag1 i)

    -- Recurse (do not distribute!)
    -- {..., tag[v₁,...,vᵢ,...],...} ↓ tag ↓ i =  vᵢ
    | Union v2 v3 <- v1 = Pick (on mkUnion go v2 v3) tag1 i

    -- Recurse
    | EVAL{} <- v1 = Pick (go v1) tag1 i
    | FETCH{} <- v1 = Pick (go v1) tag1 i
    | Pick{} <- v1 = Pick (go v1) tag1 i
    | Abs{} <- v1 = Pick (go v1) tag1 i

    | Bas <- v1 = __IMPOSSIBLE__
    | Loc{} <- v1 = __IMPOSSIBLE__

    where
      isWrong (VNode tag2 _) = tag1 /= tag2
      isWrong Bas            = True
      isWrong Loc{}          = True
      isWrong EVAL{}         = False
      isWrong FETCH{}        = False
      isWrong Pick{}         = False
      isWrong Abs{}          = False
      isWrong Union{}        = False

      isCorrect (VNode tag2 vs) | tag1 == tag2 =
        Just $ fromMaybe __IMPOSSIBLE__ $ vs !!! i
      isCorrect _ = Nothing

  go (FETCH v1)
    | Loc loc <- v1 = fromMaybe (FETCH v1) $ heapLookup loc

    -- Solve locations
    | Union{} <- v1
    , (loc : locs, v2s) <- mapMaybeAndRest isLocation $ List1.toList $ valueToList v1 =
      let v3s = List1.map (fromMaybe __IMPOSSIBLE__ . heapLookup) $ loc :| locs in
      caseList v2s
        (listToValue v3s)
        (\v2 v2s -> listToValue v3s `mkUnion` FETCH (listToValue $ v2 :| v2s))

    -- Recurse
    | EVAL{} <- v1 = FETCH $ go v1
    | FETCH{} <- v1 = FETCH $ go v1
    | Pick{} <- v1 = FETCH $ go v1
    | Abs{} <- v1 = FETCH $ go v1

    -- Distribute FETCH
    | Union v2 v3 <- v1 = on mkUnion FETCH v2 v3

    | Bas <- v1 = __IMPOSSIBLE__
    | VNode{} <- v1 = __IMPOSSIBLE__

    where
      isLocation :: Value -> Maybe Loc
      isLocation (Loc loc) = Just loc
      isLocation _         = Nothing

  go (EVAL v1)
    | VNode CTag{} _ <- v1 = v1

    -- Solve function tags by substituting their return varaible.
    | VNode FTag{tDef} _ <- v1 =
      maybe __IMPOSSIBLE__ Abs $ defReturnLookup tDef
    | VNode PTag{tDef} _ <- v1 =
      maybe __IMPOSSIBLE__ Abs $ defReturnLookup tDef

    -- Solve C nodes
    | Union{} <- v1
    , (v2:v2s, v3s) <- mapMaybeAndRest isCNode $ List1.toList $ valueToList v1 =
      caseList v3s
        (listToValue $ v2 :| v2s)
        (\v3 v3s -> listToValue (v2 :| v2s) `mkUnion` EVAL (listToValue $ v3 :| v3s))

    -- Solve F and P nodes
    | Union{} <- v1
    , (v2 : v2s, v3s) <- mapMaybeAndRest hasDefName $ List1.toList $ valueToList v1 =
      let v2s' = List1.map (maybe __IMPOSSIBLE__ Abs . defReturnLookup) (v2 :| v2s) in
      caseList v3s
        (listToValue v2s')
        (\v3 v3s -> listToValue v2s' `mkUnion` EVAL (listToValue $ v3 :| v3s))

    -- Recurse
    | EVAL{} <- v1 = EVAL $ go v1
    | FETCH{} <- v1 = EVAL $ go v1
    | Pick{} <- v1 = EVAL $ go v1
    | Abs{} <- v1 = EVAL $ go v1

    -- Distribute
    | Union v2 v3 <- v1 = on mkUnion EVAL v2 v3

    | Bas <- v1 = __IMPOSSIBLE__
    | Loc{} <- v1 = __IMPOSSIBLE__

    where
      isCNode (VNode tag@CTag{} vs) = Just $ VNode tag vs
      isCNode _                     = Nothing

      hasDefName (VNode PTag{tDef} _) = Just tDef
      hasDefName (VNode FTag{tDef} _) = Just tDef
      hasDefName _                    = Nothing

locHeapUpdate :: Loc -> Value -> AbstractContext -> AbstractContext
locHeapUpdate loc v cxt =
    cxt{fHeap = AbsHeap heap}
  where
    heap = for (unAbsHeap cxt.fHeap) $
      \(loc', v') -> if loc == loc' then (loc', v) else (loc', v')

absEnvUpdate :: Abs -> Value -> AbstractContext -> AbstractContext
absEnvUpdate abs v cxt =
    cxt{fEnv = AbsEnv env}
  where
    env = for (unAbsEnv cxt.fEnv) $
      \(abs', v') -> if abs == abs' then (abs', v) else (abs', v')

envInsert :: (Abs, Value) -> AbstractContext -> AbstractContext
envInsert entry cxt = cxt{fEnv = AbsEnv $ insert entry $ unAbsEnv cxt.fEnv}

heapInsert :: (Loc, Value) -> AbstractContext -> AbstractContext
heapInsert entry cxt = cxt{fHeap = AbsHeap $ insert entry $ unAbsHeap cxt.fHeap}
