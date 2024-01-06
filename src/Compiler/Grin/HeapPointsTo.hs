{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns             #-}

-----------------------------------------------------------------------
-- | * GRIN heap points-to analysis
-----------------------------------------------------------------------

-- • "determine for each call to eval, a safe approximation to what different node
--   values (or rather tags) that eval might find when it fetches a node from the
--   heap via its argument pointer." (s. 67)
--
-- • The abstract heap (also called store) maps locations to a set of nodes.
--   Locations are defined as {1, 2, ...,maxloc} where maxloc is total number
--   of store operations. Additionally, locations with F-tags also contain the
--   possible return types of the function. This is due to the eval function
--   which will be generated later, but has the ability to @update@ the node with
--   the evaluated value.
--
-- • The Abstract enviroment maps variables to a set of values.
--
-- • The heap points-to analysis also incorporates a sharing analysis,
--   which determines for each abstract location if it is shared or unique.
--   An abstract location is shared if a concrete instance of the abstract
--   location is subject to @fetch@ more than once. In practice, this is
--   evident if the location is a possible value of a variable which is used
--   more than once.

module Compiler.Grin.HeapPointsTo
  ( heapPointsTo
  ) where

import           Control.Monad                  (join)
import           Control.Monad.Reader           (MonadReader (ask, local),
                                                 Reader, ReaderT, asks,
                                                 runReader)
import           Control.Monad.State            (State, evalState, gets, modify)
import           Data.Bifunctor                 (Bifunctor (bimap))
import           Data.Foldable                  (fold, foldrM, toList)
import           Data.Function                  (on)
import           Data.List                      (find, insert, intercalate,
                                                 intersectBy, nub, partition,
                                                 sortOn, (\\))
import           Data.List.Extra                (firstJust)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Tuple.Extra               (both)
import           GHC.IO                         (unsafePerformIO)
import           Prelude                        hiding ((!!))

import           Agda.Syntax.Common.Pretty
import           Agda.Utils.Function            (applyWhen)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1               (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1               as List1
import           Agda.Utils.Maybe


import           Compiler.Grin.Grin             hiding (cnat)
import           Compiler.Grin.HeapPointsToType
import qualified Compiler.Grin.SolveEquations   as Solve
import           Utils.Utils


-- TODO
-- • Refactor H', HCxt, HRet

-- FIXME
-- PAdd 3 0 ; λ x13 →
-- >>>
-- x13 → Cnat [BAS]
--

heapPointsTo :: [GrinDefinition] -> (AbstractContext, AbstractContext, Set Gid)
heapPointsTo defs = (set lensAbsHeap equations.absHeap'.unAbsHeap absCxt, absCxt', shared) where
  absCxt' = sortAbsCxt $ {- Solve. -}solveEquations defs absCxt
  absCxt = sortAbsCxt $ AbstractContext absHeap equations.absEnv'
  absHeap = applySharingAnalyis shared equations.absHeap'
  shared = equations.shared'
  equations =
    foldl1 (<>) $ for defs $ \def ->
      runReader (deriveEquations def.gr_term) $
      initHCxt defs def


applySharingAnalyis :: Set Gid -> AbsHeap -> AbsHeap
applySharingAnalyis shared = over lensUnAbsHeap (map go) where
    go (loc, v)
      | Set.member loc.unLoc shared = (loc, v)
      | otherwise = (loc, removeVariables v)

    removeVariables v =
      caseList (List1.filter notVariable $ valueToList v)
        v
        (\v vs -> listToValue $ v :| vs)

    notVariable = \case
      Abs{}   -> False
      Union{} -> __IMPOSSIBLE__
      _       -> True

newtype Multiplicities = Multiplicities{unMultiplicities :: Map Abs Int}

instance Semigroup Multiplicities where
  (<>) x = Multiplicities . on (Map.unionWith (+)) unMultiplicities x

instance Monoid Multiplicities where
  mempty = Multiplicities mempty

instance Pretty Multiplicities where
  pretty (Multiplicities ms) =
      vcat $ map prettyEntry $ Map.toList ms
    where
      prettyEntry (x, n) =
        text (prettyShow x ++ ":") <+> pretty n

countMultiplicities :: GrinDefinition -> Multiplicities
countMultiplicities def = evalState (go def.gr_term) def.gr_args where
  go :: Term -> State [Abs] Multiplicities
  go (Case v t alts) =
    goVal v <> foldrM (\alt m -> pure m <> goCalt alt) mempty alts <> go t
  go (Bind t alt) = go t <> goLalt alt
  go (Store _ v) = goVal v
  go (Unit v) = goVal v
  go (App v vs) = goVal v <> foldrM (\v m -> pure m <> goVal v) mempty vs
  go (Fetch' _ n _) = gets $ (!! n) <&> \abs -> Multiplicities $ Map.singleton abs 1
  go (Update _ n v) = do
    m <- gets $ (!! n) <&> \abs -> Multiplicities $ Map.singleton abs 1
    pure m <> goVal v
  go (Error _) = pure mempty

  goLalt :: LAlt -> State [Abs] Multiplicities
  goLalt (LAltVar abs t)           = modify (abs:) *> go t
  goLalt (LAltConstantNode _ xs t) = modify (reverse xs ++) *> go t
  goLalt (LAltEmpty t)             = go t


  goCalt :: CAlt -> State [Abs] Multiplicities
  goCalt (CAltConstantNode _ xs t) = modify (reverse xs ++) *> go t
  goCalt (CAltLit _ t)             = go t



  goVal :: Val -> State [Abs] Multiplicities
  goVal (ConstantNode _ vs) = foldrM (\v m -> pure m <> goVal v) mempty vs
  goVal (VariableNode n vs) = do
    m <- gets $ (!! n) <&> \abs -> Multiplicities $ Map.singleton abs 1
    pure m <> foldrM (\v m -> pure m <> goVal v) mempty vs
  goVal (Tag _) = pure mempty
  goVal (Var n) = gets $ (!! n) <&> \abs -> Multiplicities $ Map.singleton abs 1
  goVal (Lit _) = pure mempty
  goVal (Def _) = pure mempty
  goVal (Prim _) = pure mempty
  goVal Empty = pure mempty


sortAbsCxt :: AbstractContext -> AbstractContext
sortAbsCxt cxt = cxt{fHeap = sortAbsHeap cxt.fHeap, fEnv = sortAbsEnv cxt.fEnv}

sortAbsHeap :: AbsHeap -> AbsHeap
sortAbsHeap (AbsHeap heap) = AbsHeap $ sortOn fst heap

sortAbsEnv :: AbsEnv -> AbsEnv
sortAbsEnv (AbsEnv env) = AbsEnv $ sortOn fst env

-- TODO add VTag?


type H' = Reader HCxt

data HCxt = HCxt
  { absHeap    :: AbsHeap
  , absEnv     :: AbsEnv
  , defs       :: [GrinDefinition]
  , abss       :: [Abs]
  , locs       :: [Loc]
  , shared     :: Set Gid
  , currentDef :: GrinDefinition
  }

initHCxt :: [GrinDefinition] -> GrinDefinition -> HCxt
initHCxt defs currentDef =
    HCxt
      { absHeap = AbsHeap []
      , absEnv = AbsEnv []
      , defs = defs
      , abss = reverse currentDef.gr_args
      , locs = []
      , shared = shared
      , currentDef = currentDef
      }
  where
    shared =
      Set.fromList $
      mapMaybe (\(MkAbs gid, n) -> boolToMaybe (n > 1) gid) $
      Map.toList $
      unMultiplicities $
      foldl (\ms def -> ms <> countMultiplicities def) mempty defs


instance Semigroup HRet where
  h1 <> h2 =
    let heap = on (<>) absHeap' h1 h2
        env = on (<>) absEnv' h1 h2 in
    HRet
      { absHeap' = heap
      , absEnv'  = env
      , shared' = on (composeShared heap env) shared' h1 h2
      }

data HRet = HRet
  { absHeap' :: AbsHeap
  , absEnv'  :: AbsEnv
  , shared'  :: Set Gid
  }

composeShared :: AbsHeap -> AbsEnv -> Set Gid -> Set Gid -> Set Gid
composeShared heap env = updateShared heap env .: Set.union

updateShared :: AbsHeap -> AbsEnv -> Set Gid -> Set Gid
updateShared absHeap absEnv shared
  | shared' == shared = shared
  | otherwise = updateShared absHeap absEnv shared'
  where
    shared' = foldl addGids shared vs
    vs =
      nub $
      mapMaybe (\(MkLoc gid, v) -> boolToMaybe (Set.member gid shared) v) (unAbsHeap absHeap) ++
      mapMaybe (\(MkAbs gid, v) -> boolToMaybe (Set.member gid shared) v) (unAbsEnv absEnv)

makeShared :: AbsHeap -> AbsEnv -> Set Gid -> Value -> Set Gid
makeShared heap env = updateShared heap env .: addGids

addGids :: Set Gid -> Value -> Set Gid
addGids s (Loc (MkLoc gid)) = Set.insert gid s
addGids s (Abs (MkAbs gid)) = Set.insert gid s
addGids s (VNode _ vs)      = foldl addGids s vs
addGids s Bas               = s
addGids s (Union v1 v2)     = addGids (addGids s v1) v2
addGids s (Pick v _ _)      = addGids s v
addGids s (EVAL v)          = addGids s v
addGids s (FETCH v)         = addGids s v

localAbs :: MonadReader HCxt m => Abs -> m a -> m a
localAbs abs = local $ \cxt -> cxt{abss = abs : cxt.abss}

localAbss :: MonadReader HCxt m => [Abs] -> m a -> m a
localAbss = foldl (.: localAbs) id

localLoc :: MonadReader HCxt m => Loc -> m a -> m a
localLoc loc = local $ \cxt -> cxt{locs = loc : cxt.locs}

-- TODO use lenses?
-- | Adds l → v to the abstract heap and makes v shared if x is shared
localAbsHeap :: MonadReader HCxt m => (Loc, Value) -> m a -> m a
localAbsHeap (loc, v) = local $ \cxt ->
  let heap = unAbsHeap cxt.absHeap
      heap'
        | any ((==loc) . fst) heap =
          for heap $ \(loc', v') ->
            if loc == loc' then (loc', mkUnion v' v ) else (loc', v')
        | otherwise = insert (loc, v) $ unAbsHeap cxt.absHeap
      absHeap = AbsHeap heap' in

  applyWhen
    (Set.member (unLoc loc) cxt.shared)
    (\cxt -> cxt{shared = makeShared absHeap cxt.absEnv cxt.shared v})
    cxt{absHeap = absHeap}

-- | Adds x → v to the abstract enviroment and makes v shared if x is shared
localAbsEnv :: MonadReader HCxt m => (Abs, Value) -> m a -> m a
localAbsEnv (abs, v) = local $ \cxt ->
  let env = unAbsEnv cxt.absEnv
      env'
        | any ((==abs) . fst) env =
          for env $ \(abs', v') ->
            if abs == abs' then (abs', mkUnion v' v ) else (abs', v')
        | otherwise = insert (abs, v) env
      absEnv' = AbsEnv env' in

  applyWhen
    (Set.member (unAbs abs) cxt.shared)
    (\cxt -> cxt{shared = makeShared cxt.absHeap absEnv' cxt.shared v})
    cxt{absEnv = absEnv'}

localAbssEnv :: MonadReader HCxt m => [(Abs, Value)] -> m a -> m a
localAbssEnv = foldl (.: localAbsEnv) id

deriveEquations :: Term -> H' HRet
deriveEquations term = case term of
    App (Def "printf") _ -> retHCxt

    Store loc (ConstantNode tag vs) `Bind` LAltVar x t -> do
      vs' <- mapM valToValue vs
      case tag of
        FTag name _ -> do
          callee <- asks (fromMaybe __IMPOSSIBLE__ . find ((name==) . gr_name) . defs)
          let callee_return = fromMaybe __IMPOSSIBLE__ callee.gr_return

          localLoc loc $
            localAbs x $
            localAbsEnv (x, Loc loc) $
            localAbsHeap (loc, VNode tag (toList vs') `mkUnion` Abs callee_return) $
            localAbssEnv (zip callee.gr_args $ toList vs') $
            deriveEquations t
        CTag _ _ ->
          localLoc loc $
          localAbs x $
          localAbsEnv (x, Loc loc) $
          localAbsHeap (loc, VNode tag $ toList vs') $
          deriveEquations t

        PTag{} -> __IMPOSSIBLE__
      where
      valToValue :: MonadReader HCxt m => Val -> m Value
      valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      valToValue (Lit _) = pure Bas
      valToValue  _      = __IMPOSSIBLE__

    Store loc (Var n) `Bind` LAltVar x t -> do
      v <- Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      localLoc loc $
        localAbs x $
        localAbsEnv (x, Loc loc) $
        localAbsHeap (loc, v) $
        deriveEquations t

    Bind (App (Def "eval") [Var n]) (LAltVar x (Case (Var 0) t alts)) -> do
      v <- EVAL . FETCH . Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      localAbs x $ localAbsEnv (x, v) $ do
        hs <- mapM (deriveEquationsAlt x) alts
        h <- deriveEquations t
        pure $ foldl (<>) h hs
      where
        deriveEquationsAlt :: Abs -> CAlt -> H' HRet
        deriveEquationsAlt x (CAltConstantNode tag xs t) =
          localAbss xs $
          localAbssEnv (zip xs $ map (Pick (Abs x) tag) [0 ..]) $
          deriveEquations t
        deriveEquationsAlt _ CAltLit{}      = __IMPOSSIBLE__
        deriveEquationsAlt _ _ = __IMPOSSIBLE__


    -- should never happen?
    Bind (App (Def "eval") [Var n]) (LAltVar x t) -> do
      v <- EVAL . FETCH . Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      localAbs x $
        localAbsEnv (x, v) $
        deriveEquations t

    -- Returning eval
    App (Def "eval") [Var n] -> do
      v <- EVAL . FETCH . Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      x <- asks $ fromMaybe __IMPOSSIBLE__ . gr_return . currentDef
      localAbs x $ localAbsEnv (x, v) retHCxt

    Bind (App (Def "eval") [Var _]) (LAltConstantNode tag [x] (Case (Var 0) t alts)) | tag == natTag ->
      localAbs x $
      localAbsEnv (x, Bas) $ do
        hs <- mapM deriveEquationsAlt alts
        h <- deriveEquations t
        pure $ foldl (<>) h hs
      where
        deriveEquationsAlt (CAltLit _ t) = deriveEquations t
        deriveEquationsAlt _             = __IMPOSSIBLE__

    Bind (App (Def "eval") [Var _]) (LAltConstantNode tag [x] t) | tag == natTag ->
      localAbs x $
      localAbsEnv (x, Bas) $
      deriveEquations t

    Bind (App (Def defName) vs) (LAltVar x t) -> do
      def <- asks $ fromMaybe __IMPOSSIBLE__ . find ((defName==) . gr_name) . defs
      vs' <- mapM valToValue vs
      localAbs x $
        localAbsEnv (x, maybe __IMPOSSIBLE__ Abs def.gr_return) $
        localAbssEnv (zip def.gr_args vs') $
        deriveEquations t
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
        valToValue (Lit _) = pure Bas
        valToValue  _      = __IMPOSSIBLE__

    App (Def defName) vs -> do
        name <- asks $ gr_name. currentDef
        caller_return <- asks (fromMaybe (error $ "no return: " ++ name) . gr_return . currentDef)
        callee <- asks $ fromMaybe (error $ "can't find " ++ defName) . find ((defName==) . gr_name) . defs
        let callee_return = fromMaybe __IMPOSSIBLE__ callee.gr_return
        vs' <- mapM valToValue vs
        -- TODO this is correct now but it causes non-termination when solving the equations
        localAbssEnv ((caller_return, Abs callee_return) : zip callee.gr_args vs') retHCxt
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
        valToValue (ConstantNode tag vs) = VNode tag . toList <$> mapM valToValue vs
        valToValue (Lit _) = pure Bas
        valToValue (Tag tag) = pure $ VNode tag []
        valToValue _ = __IMPOSSIBLE__

    Bind (App (Def defName) vs) (LAltConstantNode tag xs t) -> do
      def <- asks $ fromMaybe __IMPOSSIBLE__ . find ((defName==) . gr_name) . defs
      let gr_return = maybe __IMPOSSIBLE__ Abs def.gr_return
      mapM valToValue vs >>= \vs' ->
        localAbss xs $
        localAbssEnv (zip xs $ map (Pick gr_return tag) [0 ..]) $
        localAbssEnv (zip def.gr_args vs') $
        deriveEquations t
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
        valToValue (Lit _) = pure Bas
        valToValue  _      = __IMPOSSIBLE__

    Bind (App (Prim _) _) (LAltVar x t)  ->
      localAbs x $
      localAbsEnv (x, Bas) $
      deriveEquations t

    Unit v -> do
        x <- asks $ fromMaybe __IMPOSSIBLE__ . gr_return . currentDef
        v' <- valToValue v
        localAbs x $ localAbsEnv (x, v') retHCxt
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
        valToValue (ConstantNode tag vs) = VNode tag . toList <$> mapM valToValue vs
        valToValue (Lit _) = pure Bas
        valToValue (Tag tag) = pure $ VNode tag []
        valToValue _ = __IMPOSSIBLE__

    Error _ -> retHCxt
    t -> error $ "NOT IMPLEMENTED " ++ show t

retHCxt :: MonadReader HCxt m => m HRet
retHCxt =
  ask <&> \cxt ->
    HRet {absHeap' = cxt.absHeap, absEnv' = cxt.absEnv, shared' = cxt.shared}

deBruijnLookup :: MonadReader HCxt m => Int -> m (Maybe Abs)
deBruijnLookup n = asks $ (!!! n) . abss


solveEquations :: [GrinDefinition] -> AbstractContext -> AbstractContext
solveEquations defs AbstractContext{fHeap = AbsHeap heapEqs, fEnv = AbsEnv envEqs} =
    fix initAbstractContext
  where
    initAbstractContext :: AbstractContext
    initAbstractContext = mempty{fEnv = AbsEnv $ map step $ mapMaybe gr_return defs}
      where
      step abs = (abs, fromMaybe __IMPOSSIBLE__ $ envEqsLookup abs)

    envEqsLookup :: Abs -> Maybe Value
    envEqsLookup abs = lookup abs envEqs

    -- trace'' s x = unsafePerformIO $ do
    --   putStrLn s
    --   pure x

    fix :: AbstractContext -> AbstractContext
    fix cxt
      | cxt == cxt' = cxt'
      | otherwise   = fix cxt'
      where
        cxt' =
          caseMaybe (nextEnvEq cxt) cxt $
            \entry -> fixCurrent $ addMissingEqs $ envInsert entry cxt

        fixCurrent :: AbstractContext -> AbstractContext
        fixCurrent cxt
          | cxt == cxt' = cxt'
          | otherwise = fixCurrent {- $ trace'' ("\n" ++ prettyShow cxt') -} cxt'
          where
            -- Simplify and update each entry in abstract heap and enviroment
            cxt' =
              let cxt' =
                    foldl
                      (\cxt (l, v) -> absHeapUpdate l (simplify defs cxt v) cxt)
                      cxt
                      (unAbsHeap cxt.fHeap) in
              foldl
                (\cxt (x, v) -> absEnvUpdate x (simplify defs cxt v) cxt)
                cxt'
                (unAbsEnv cxt'.fEnv)

    -- | Returns the next missing abstract heap equation (e.g. `x21 → l4 ∪ l24`).
    nextEnvEq :: AbstractContext -> Maybe (Abs, Value)
    nextEnvEq =
      listToMaybe . differenceBy (on (==) fst) envEqs . unAbsEnv . fEnv

    -- | Adds missing equations that are referenced in the current equations.
    addMissingEqs :: AbstractContext -> AbstractContext
    addMissingEqs cxt
      | cxt == cxt' = cxt'
      | otherwise = addMissingEqs cxt'
      where
        cxt' =
          foldr (<>) cxt $
            mapMaybe (collectEqs cxt) $
              map snd (unAbsHeap cxt.fHeap) ++ map snd (unAbsEnv cxt.fEnv)

    -- | Collect all equations refererenced equations.
    collectEqs :: AbstractContext -> Value -> Maybe AbstractContext
    collectEqs cxt (Union v1 v2) = on (<>) (collectEqs cxt) v1 v2
    collectEqs cxt (Abs abs)
      | Just _ <- lookup abs (unAbsEnv cxt.fEnv) = Nothing
      | otherwise = Just $ AbstractContext {fHeap=AbsHeap [], fEnv = AbsEnv [(abs, v)]}
      where
        v = fromMaybe (error $ "can't find " ++ prettyShow abs) $ lookup abs envEqs

    collectEqs cxt (Loc loc)
      | Just _ <- lookup loc (unAbsHeap cxt.fHeap) = Nothing
      | otherwise = Just $ AbstractContext {fHeap = AbsHeap [(loc, v)], fEnv = AbsEnv []}
      where
        v = fromMaybe __IMPOSSIBLE__ $ lookup loc heapEqs

    collectEqs cxt (FETCH v) = collectEqs cxt v
    collectEqs cxt (EVAL v1) =  collectEqs cxt v1
    collectEqs cxt (VNode _ vs) = foldl (<>) Nothing $ map (collectEqs cxt) vs
    collectEqs cxt (Pick v _ _) = collectEqs cxt v
    collectEqs _ Bas = Nothing

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
  go (Abs abs) = fromMaybe __IMPOSSIBLE__ $ envLookup abs
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
      isSelfReference _                           = False

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
    | Loc loc <- v1 = fromMaybe __IMPOSSIBLE__ $ heapLookup loc

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

absHeapUpdate :: Loc -> Value -> AbstractContext -> AbstractContext
absHeapUpdate loc v cxt =
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

