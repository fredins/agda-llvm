-- {-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Compiler.Grin.GrinTransformations
  (module Compiler.Grin.GrinTransformations) where

import           Control.Applicative            (liftA2)
import           Control.Monad                  (forM, (<=<))
import           Control.Monad.Reader           (MonadReader (local),
                                                 ReaderT (runReaderT), asks)
import           Control.Monad.State            (StateT (runStateT), modify)
import           Control.Monad.Trans.Maybe      (MaybeT (..), hoistMaybe)
import           Data.Foldable                  (find, fold, toList)
import           Data.Function                  (on)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Prelude                        hiding (drop, (!!))

import           Agda.Compiler.Backend          hiding (Prim)
import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible          (__IMPOSSIBLE__, impossible)
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1               (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1               as List1
import           Agda.Utils.Maybe

import           Compiler.Grin.Grin             as G
import           Compiler.Grin.HeapPointsToType
import           Utils.Utils

newtype TagInfo = TagInfo{unTagInfo :: Map Abs (Set Tag)}

lensTagInfo :: Lens' TagInfo (Map Abs (Set Tag))
lensTagInfo f tagInfo = TagInfo <$> f tagInfo.unTagInfo

tagInfoInsert :: Abs -> Set Tag -> TagInfo -> TagInfo
tagInfoInsert = over lensTagInfo .: Map.insert

lookupMaxArity :: Abs -> TagInfo -> Maybe Int
lookupMaxArity x tagInfo =
  maximum . map tArity . Set.toList <$> Map.lookup x tagInfo.unTagInfo

singletonTag :: Abs -> TagInfo -> Maybe Tag
singletonTag abs tagInfo =
  Map.lookup abs tagInfo.unTagInfo >>= \set ->
    boolToMaybe (Set.size set == 1) (Set.elemAt 0 set)

instance Pretty TagInfo where
  pretty (TagInfo tagInfo) =
      vcat $ map prettyEntry $ Map.toList tagInfo
    where
      prettyEntry :: (Abs, Set Tag) -> Doc
      prettyEntry (x, tags) =
            pretty x
        <+> text "→"
        <+> pretty tags

initTagInfo :: AbstractContext -> TagInfo
initTagInfo absCxt =
  TagInfo $ Map.fromList $ forMaybe (absCxt ^. lensAbsEnv) $ \(abs, v) ->
    case v of
      VNode tag _ -> Just (abs, Set.singleton tag)
      v@Union{} ->
        let tags = flip List1.mapMaybe (valueToList v) $ \case
                   VNode tag _ -> Just tag
                   _           -> Nothing in
        caseList tags Nothing $ \t ts -> Just (abs, Set.fromList $ t : ts)
      Loc _ -> Nothing
      Bas -> Nothing
      _ -> __IMPOSSIBLE__

valueToTags :: Value -> Maybe (Set Tag)
valueToTags = \case
      VNode tag _ -> Just $ Set.singleton tag
      v@Union{} ->
        let tags = flip List1.mapMaybe (valueToList v) $ \case
                   VNode tag _ -> Just tag
                   _           -> Nothing in
        caseList tags Nothing $ \t ts -> Just $ Set.fromList $ t : ts
      Loc _ -> Nothing
      Bas -> Nothing
      _ -> __IMPOSSIBLE__

type E m = ReaderT [Abs] (StateT TagInfo m)

-- | Specialize and inline calls to eval
--
-- Abstract Heap:
--   l1  → FAgda.Builtin.Nat._-_ [l1 ∪ l18, l0] ∪ Cnat [BAS]
--   l18 → Cnat [BAS]
-- Abstract Enviroment:
--   x7 → BAS
--   x8 → l1 ∪ l18
--
-- DownFrom.downFrom r9 x8 =
--   eval 0 ; λ Cnat x7 →
--
-- >>>
--
-- DownFrom.downFrom r9 x8 =
--   (fetch 0 ; λ x38 →
--    (case 0 of
--       FAgda.Builtin.Nat._-_ x40 x41 → Agda.Builtin.Nat._-_ 1 0
--       Cnat x42 → unit (Cnat 0)
--    ) ; λ x39 →
--    update 2 0 ; λ () →
--    unit 0
--   ) ; λ Cnat x7 →
--
-- Tag Info:
--   x38 → {Cnat, FAgda.Builtin.Nat._-_}
--   x39 → {Cnat}
inlineEval :: forall mf. MonadFresh Int mf
           => [GrinDefinition]
           -> AbstractContext
           -> TagInfo
           -> mf ([GrinDefinition], TagInfo)
inlineEval defs absCxt tagInfo =
  flip runStateT tagInfo $ forM defs $ \def -> do
  t <- runReaderT (go def.gr_term) (reverse def.gr_args)
  pure (setGrTerm t def)
  where
  go :: Term -> E mf Term
  go = \case
    App (Def "eval") [Var n] `Bind` LAltVar x t -> do
      (eval, returnTags) <- generateEval n
      modify (tagInfoInsert x returnTags)
      t' <- varLocal x (go t)
      pure $ eval `Bind` LAltVar x t'
    App (Def "eval") [Var n] -> do
      fst <$> generateEval n
    Case v t alts -> do
      t' <- go t
      alts' <- forM alts $ \(splitCaltWithVars -> (mkAlt, t, xs)) -> mkAlt <$> varsLocal xs (go t)
      pure (Case v t' alts')
    Bind t1 (splitLaltWithVars -> (mkAlt, t2, xs)) -> do
      t1' <- go t1
      alt <- mkAlt <$> varsLocal xs (go t2)
      pure (t1' `Bind` alt)
    term -> pure term

  generateEval :: Int -> E mf (Term, Set Tag)
  generateEval n = do
    x <- deBruijnLookup n
    case Set.toList (fetchTagSet x) of
      []         -> __IMPOSSIBLE__
      [tag]      -> genLambda tag
      (tag:tags) -> genCase (tag :| tags)
    where
    -- TODO implement this as an optimization instead
    genLambda tag = do
      x <- freshAbs

      -- Assign returnTags to x
      --
      -- fetch 0 ; λ FDownFrom.downFrom x44 →
      --   DownFrom.downFrom 0 ; λ x →
      --   update 2 0 ; λ () →
      --   unit 0
      modify (tagInfoInsert x returnTags)

      let infixr 2 `bindConstantNode`, `bindVar'`
          -- arity + var
          offset = tagArity tag + 1
          bindConstantNode t1 t2 = Bind t1 <$> laltConstantNode tag t2
          bindVar' t1 t2 = t1 `Bind` LAltVar x t2
          eval =
            Fetch tag n                         `bindConstantNode`
            genBody tag                         `bindVar'`
            Update Nothing (n + offset) (Var 0) `BindEmpty`
            Unit (Var 0)

      eval <&> (, returnTags)
      where
      returnTags = mkReturnTags (tag :| [])

    genCase tags = do
      x1 <- freshAbs
      x2 <- freshAbs

      -- Assign caseTags to x1 and returnTags to x2
      --
      -- fetch 0 ; λ x1 →
      --  (case 0 of
      --     FAgda.Builtin.Nat._-_ x40 x41 → Agda.Builtin.Nat._-_ 1 0
      --     Cnat x42 → unit (Cnat 0)
      --  ) ; λ x2 →
      modify (tagInfoInsert x2 returnTags . tagInfoInsert x1 caseTags)

      let
        infixr 2 `bindVarL'`, `bindVarR'`
        bindVarR' t1 t2 = Bind t1 . LAltVar x1 <$> t2
        bindVarL' t1 t2 = t1 <&> (`Bind` LAltVar x2 t2)
        eval =
          FetchOpaque n                     `bindVarR'`
          Case (Var 0) Unreachable <$> alts `bindVarL'`
          Update Nothing (n + 2) (Var 0)    `BindEmpty`
          Unit (Var 0)

      eval <&> (, returnTags)
      where
      caseTags = Set.fromList (toList tags)
      returnTags = mkReturnTags tags
      alts = mapM (caltConstantNode <*> genBody) (toList tags)

    genBody FTag{tDef, tArity} = App (Def tDef) $ map Var $ downFrom tArity
    genBody tag@CTag{tArity = map Var . downFrom -> vs} = Unit (ConstantNode tag vs)
    genBody _ = __IMPOSSIBLE__

  fetchTagSet :: Abs -> Set Tag
  fetchTagSet x = maybe (error $ "Can't find: " ++ prettyShow x) (foldMap filterTags) . (mapM heapLookup <=< fetchLocations) $ x

  fetchLocations :: Abs -> Maybe (List1 Loc)
  fetchLocations x = do
    vs <- valueToList <$> envLookup x
    forM vs $ \case
      Loc loc -> Just loc
      _       -> Nothing

  filterTags :: Value -> Set Tag
  filterTags (valueToList -> vs) =
    Set.fromList $ forMaybe (toList vs) $
      \case
        VNode tag _ -> Just tag
        _           -> Nothing

  mkReturnTags :: List1 Tag -> Set Tag
  mkReturnTags vs = Set.fromList $ toList $ foldMap go vs
    where
    go = \case
      FTag name _ -> maybe __IMPOSSIBLE__ filterTags $ envLookup =<< gr_return =<< find ((name==) . gr_name) defs
      PTag{} -> __IMPOSSIBLE__
      ctag -> Set.singleton ctag

  varLocal :: Abs -> E mf a -> E mf a
  varLocal abs = local (abs :)

  varsLocal :: [Abs] -> E mf a -> E mf a
  varsLocal = foldl (.: varLocal) id

  heapLookup :: Loc -> Maybe Value
  heapLookup loc = lookup loc $ unAbsHeap absCxt.fHeap

  envLookup :: Abs -> Maybe Value
  envLookup abs = lookup abs $ unAbsEnv absCxt.fEnv

  deBruijnLookup :: Int -> E mf Abs
  deBruijnLookup n = asks $ fromMaybe __IMPOSSIBLE__ . (!!! n)

-- unit v ; λ x →
-- <m>
-- >>>
-- <m> [x / v]
-- TODO fill in patterns
leftUnitLaw :: Term -> Term
-- unit n ; λ xₘ →
-- 〈t 〉
-- >>>
-- 〈t 〉[n / m]
leftUnitLaw (Unit (Var n) `Bind` LAltVar _ t) = strengthen impossible (applySubst (inplaceS 0 $ Var $ succ n) $ leftUnitLaw t)
-- unit (tag v₁ v₂) ; λ tag x₁ x₂ →
-- 〈t 〉
-- >>>
-- 〈t 〉[v₁ / 1, v₂ / 0]
leftUnitLaw (Unit (ConstantNode tag1 vs) `Bind` LAltConstantNode tag2 xs t)
  | tag1 == tag2 = t''
  | otherwise    = __IMPOSSIBLE__
  where
  numSubst = length xs
  vs' = raise numSubst $ reverse (take numSubst vs)
  -- Substitute away pattern variable usage
  t' = foldr applySubst (leftUnitLaw t) (zipWith inplaceS [0 ..] vs')
  -- Remove pattern variables by strengthening
  t'' = applySubst (strengthenS impossible numSubst) t'
-- -- unit (tag v₁ v₂) ; λ x →
-- -- 〈t 〉
-- -- >>>
-- -- 〈t 〉[(tag v₁ v₂) / 0]
leftUnitLaw (Unit (ConstantNode tag vs) `Bind` LAltVar _ t) = t''
  where
  v = raise 1 (ConstantNode tag vs)
  t' = applySubst (inplaceS 0 v) $ leftUnitLaw t
  t'' = strengthen impossible t'
leftUnitLaw (Bind t1 (splitLalt -> (mkAlt, t2))) = Bind (leftUnitLaw t1) (mkAlt $ leftUnitLaw t2)
leftUnitLaw (Case n t alts) = Case n (leftUnitLaw t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (leftUnitLaw t)
leftUnitLaw t = t

-- | Normalise the GRIN expression by making the expression right-skewed.
normalise :: Term -> Term
normalise (Bind (Bind t (splitLaltWithVars -> (mkAlt1, t1, xs))) (splitLalt -> (mkAlt2, t2))) =
  normalise $ Bind t $ mkAlt1 $ Bind t1 $ raise (length xs) (mkAlt2 t2)
normalise (Bind t1 (splitLalt -> (mkAlt, t2))) = normalise t1 `Bind` mkAlt (normalise t2)
normalise (Case n t alts) = Case n (normalise t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (normalise t)
normalise t = t

data UpdateCxt = UpdateCxt
  { tagInfo :: TagInfo
  , vars    :: [Abs]
  }

initUpdateCxt :: TagInfo -> GrinDefinition -> UpdateCxt
initUpdateCxt tagInfo def = UpdateCxt{tagInfo = tagInfo, vars = reverse def.gr_args}

specializeUpdate :: forall mf. MonadFresh Int mf => TagInfo -> GrinDefinition -> mf GrinDefinition
specializeUpdate tagInfo def = do
  term <- runReaderT (go def.gr_term) (initUpdateCxt tagInfo def)
  pure (setGrTerm term def)
  where

  -- update n₁ n₂ ; λ () →
  -- 〈m₁ 〉
  -- case n₂ of
  --   CNil       → 〈m₂ 〉
  --   CCons x xs → 〈m₃ 〉
  -- >>>
  -- 〈m₁ 〉
  -- case n₂ of
  --   CNil       →
  --     updateᶜᴺⁱˡ n₁' n₂' ; λ () →
  --     〈m₂ 〉
  --   CCons x xs →
  --     updateᶜᶜᵒⁿˢ n₁' n₂' ; λ () →
  --     〈m₃ 〉
  go :: Term -> ReaderT UpdateCxt mf Term
  go (caseUpdateView -> Just (mkUpdate, m, Case t1 t2 alts)) =
    go $ m $ Case t1 t2 alts'
    where
    alts' =
      for alts $ \case
        CAltConstantNode tag xs t -> CAltConstantNode tag xs (update `BindEmpty` t)
          where update = raise (length xs) (mkUpdate tag)
        CAltTag tag t -> CAltTag tag (mkUpdate tag `BindEmpty` t)
        _ -> __IMPOSSIBLE__


  go (Update Nothing n1 (ConstantNode tag vs)) = pure $ Update (Just tag) n1 (ConstantNode tag vs)

  -- update n₁ n₂ ; λ () →
  -- unit n₂ ; λ CNat x →
  -- 〈m 〉
  -- >>>
  -- updateᶜᴺᵃᵗ n₁ n₂ ; λ () →
  -- unit n₂ ; λ CNat x →
  -- 〈m 〉
  go (Update Nothing n1 (Var n2) `BindEmpty`
      Unit (Var n2') `Bind` LAltConstantNode tag xs t) | n2 == n2' = do
      t' <- varLocals xs (go t)
      pure $
        Update (Just tag) n1 (Var n2) `BindEmpty`
        Unit (Var n2') `Bind` LAltConstantNode tag xs t'

  -- Returning eval/update [Boquist 1999, p. 95]
  -- FIXME ugly
  go (Fetch' mtag n1 moffset     `Bind` LAltVar x1
     (Case v1 t alts             `Bind` LAltVar x2
     (Update Nothing n2 (Var n3) `BindEmpty`
     (Unit (Var n3')))))
    | n3' == n3 = do
    x <- asks $ (!! n3) . ([x2, x1] ++ ) . vars
    let tags = maybe (error $ "CANNOT FIND " ++ show x) Set.toList $ Map.lookup x (unTagInfo tagInfo)
    alts' <- mapM (caltConstantNode <*> genBody) tags
    -- Rerun the transformation so it matches the case pattern
    go (Fetch' mtag n1 moffset     `Bind` LAltVar x1
       (Case v1 t alts             `Bind` LAltVar x2
       (Update Nothing n2 (Var n3) `BindEmpty`
        Case (Var n3') Unreachable alts')))
    where
      genBody FTag{tDef, tArity} = App (Def tDef) $ map Var $ downFrom tArity
      genBody tag@CTag{tArity = map Var . downFrom -> vs} = Unit (ConstantNode tag vs)
      genBody _ = __IMPOSSIBLE__

      -- fetchTagSet :: List1 Loc -> MaybeT (ReaderT UpdateCxt mf) (Set Tag)
      -- fetchTagSet locs = do
      --   heap <- asks ((^. lensAbsHeap) . absCxt)
      --   hoistMaybe (foldMap filterTags <$> mapM (`lookup` heap) locs)

      -- fetchLocations :: Abs -> MaybeT (ReaderT UpdateCxt mf) (List1 Loc)
      -- fetchLocations x = do
      --   env <- asks ((^. lensAbsEnv) . absCxt)
      --   hoistMaybe (mapM isLoc . valueToList =<< lookup x env)
      --   where
      --   isLoc = \case
      --     Loc loc -> Just loc
      --     _       -> Nothing
      --
      -- filterTags :: Value -> Set Tag
      -- filterTags (valueToList -> vs) =
      --   Set.fromList $ forMaybe (toList vs) $
      --     \case
      --       VNode tag _ -> Just tag
      --       _           -> Nothing

  go (Bind t1 (splitLaltWithVars -> (mkAlt, t2, xs))) =
    liftA2 (\t1 t2 -> t1 `Bind` mkAlt t2) (go t1) (varLocals xs (go t2))
  go (Case n t alts) = do
    alts' <- mapM (\(splitCaltWithVars -> (mkAlt, t, xs)) -> mkAlt <$> varLocals xs (go t)) alts
    t' <- go t
    pure (Case n t' alts')
  go (UpdateTag tag n v) = pure (UpdateTag tag n v)
--   go (Update Nothing n v) = error $ "UPDATE SPECIALIZATION FAILED: " ++ prettyShow (Update Nothing n v)
  go t = pure t


  varLocals :: MonadReader UpdateCxt m => [Abs] -> m a -> m a
  varLocals = foldr (\x f -> varLocal x . f) id

  varLocal :: MonadReader UpdateCxt m => Abs -> m a -> m a
  varLocal x = local $ \cxt -> cxt{vars = x : cxt.vars}

-- Preconditions: Normalised
--
-- update n₁ n₂ ; λ () →
-- 〈m 〉where n₁ ∉ m
-- case n₂ of alts
--
-- Returns: (update n₁' n₂', 〈m 〉, case n₂ of alts)
-- TODO use splitLaltWithVars
caseUpdateView :: Term -> Maybe (Tag -> Term, Term -> Term, Term)
caseUpdateView (Update Nothing n v1 `BindEmpty` t) = go IdS id t
  where
  go :: Substitution' Val
     -> (Term -> Term)
     -> Term
     -> Maybe (Tag -> Term, Term -> Term, Term)
  go rho m (Case v2 t3 alts)
    | v1' <- applySubst rho v1
    , v1' == v2 =
      let n' = case lookupS rho n of
                 Var n -> n
                 _     -> __IMPOSSIBLE__ in
      Just (\tag -> Update (Just tag) n' v1', m , Case v2 t3 alts)
    | otherwise = Nothing
  go rho m (Bind t1 (splitLaltWithVars -> (mkAlt, t2, xs))) = go rho' (m . Bind t1 . mkAlt) t2
    where
    rho' = raiseS (length xs) `composeS` rho
  go _ _ _ = Nothing

caseUpdateView _ = Nothing


-- | Replace all node variables by explicit nodes.
--
-- <t1> ; λ x₁ →
-- <t2>
-- >>>
-- <t1> ; λ tag x₂ x₃ →
-- <t2> [tag x₂ x₃ / x₁]
-- TODO update taginfo
vectorize :: forall mf. MonadFresh Int mf => TagInfo -> Term -> mf Term
vectorize tagInfo (t1 `Bind` LAltVar x1 t2)
  | Just tag <- singletonTag x1 tagInfo = do
    let arity = tagArity tag
        vs = map Var $ downFrom arity
        node = ConstantNode tag vs
        offset = arity
        rho = singletonS offset node `composeS` raiseS offset
    t2' <- vectorize tagInfo t2
    alt <- laltConstantNode tag (applySubst rho t2')
    pure (t1 `Bind` alt)


  | Just arity <- lookupMaxArity x1 tagInfo = do
    let vs = map Var $ downFrom arity
        node = VariableNode arity vs
        offset = succ arity
        rho = singletonS offset node `composeS` raiseS offset
    t2' <- vectorize tagInfo t2
    alt <- laltVariableNode arity (applySubst rho t2')
    pure (t1 `Bind` alt)

vectorize absCxt (t1 `Bind` (splitLalt -> (mkAlt, t2))) = do
  t1' <- vectorize absCxt t1
  t2' <- vectorize absCxt t2
  pure (t1' `Bind` mkAlt t2')

vectorize absCxt (Case v t alts) = do
  t' <- vectorize absCxt t
  alts' <- mapM step alts
  pure $ Case v t' alts'
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt <$> vectorize absCxt t

vectorize _ t = pure t


-- Should this be part of propagateConstant?
constantNodeUpdate :: Term -> Term
constantNodeUpdate (Update (Just tag) n (VariableNode _ vs)) =
    Update (Just tag) n $ ConstantNode tag (take (tagArity tag) vs)
constantNodeUpdate (t1 `Bind` (splitLalt -> (mkAlt, t2))) =
  constantNodeUpdate t1 `Bind` mkAlt (constantNodeUpdate t2)
constantNodeUpdate(Case v t alts) = Case v (constantNodeUpdate t) (map step alts)
  where step (splitCalt -> (mkAlt, t)) = mkAlt (constantNodeUpdate t)
constantNodeUpdate t = t


--
-- <t1>
-- case tag x₁ x₂ of
--   Cnil        → <t2>
--   Ccons x₃ x₄ → <t3>
-- >>>
-- <t1>
-- case tag of
--   Cnil  → <t2>
--   Ccons → <t3> [x₁ / x₃, x₂ / x₄]
simplifyCase :: Term -> Term
simplifyCase (Case (ConstantNode tag vs) t alts) =
  Case (Tag tag) (simplifyCase t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = substPatternBinds vs (mkAlt (simplifyCase t))
simplifyCase (Case (VariableNode n vs) t alts) =
  Case (Var n) (simplifyCase t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = substPatternBinds vs (mkAlt (simplifyCase t))
simplifyCase (Case v t alts) = Case v (simplifyCase t) $ map step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt $ simplifyCase t
simplifyCase (t1 `Bind` (splitLalt -> (mkAlt, t2))) = simplifyCase t1 `Bind` mkAlt (simplifyCase t2)
simplifyCase term = term

substPatternBinds :: [Val] -> CAlt -> CAlt
substPatternBinds vs (CAltConstantNode tag xs t) = CAltTag tag t''
  where
  numSubst = length xs
  vs' = raise numSubst $ reverse (take numSubst vs)
  -- Substitute away pattern variable usage
  t' = foldr applySubst t (zipWith inplaceS [0 ..] vs')
  -- Remove pattern variables by strengthening
  t'' = applySubst (strengthenS impossible numSubst) t'
substPatternBinds _ alt = alt


-- | Split fetch operations using offsets
-- <t1>
-- fetch n ; λ x₁ x₂ x₃ x₄ →
-- <t2>
-- >>>
-- <t1>
-- fetch n [1]; λ x₂ →
-- fetch n [2]; λ x₃ →
-- fetch n [3]; λ x₄ →
-- <t2>
-- TODO returning fetch [Boquist 1999, p. 105]
splitFetch :: Term -> Term
splitFetch (Fetch' mtag n Nothing `Bind` LAltConstantNode _ xs t) = fetchRest mtag n xs (splitFetch t)
splitFetch (Fetch' mtag n Nothing `Bind` LAltVariableNode x xs t) =
  Fetch' mtag n (Just 1) `Bind` LAltVar x (fetchRest mtag (n + 1) xs (splitFetch t))
splitFetch (t1 `Bind` (splitLalt -> (mkAlt, t2))) = splitFetch t1 `Bind` mkAlt (splitFetch t2)
splitFetch (Case v t alts) = Case v (splitFetch t) $ map step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (splitFetch t)
splitFetch term = term

fetchRest :: Maybe Tag -> Int -> [Abs] -> Term -> Term
fetchRest mtag n xs t = foldr (uncurry fetch) t (zip [2 ..] xs)
  where
  fetch offset x t = Fetch' mtag (n + offset - 2) (Just offset) `Bind` LAltVar x t



-- TODO
-- • Maybe leave fetch operations that are used by all alternatives and all uses the
--   small layout.
-- • Annotate fetch with tag so it is known whether small layout or big layout should
--   be used.
-- • Implement the the missing patterns (see TODOs below)
rightHoistFetch :: forall mf. MonadFresh Int mf => Term -> mf Term
rightHoistFetch (FetchOpaqueOffset n 1      `Bind` LAltVar x1
                (FetchOpaqueOffset _ offset `Bind` LAltVar x2 t)) = do
  t' <- hoistFetch n x1 x2 t offset
  rightHoistFetch (FetchOpaqueOffset n 1 `Bind` LAltVar x1 t')

rightHoistFetch (Bind t1 (splitLalt-> (mkAlt, t2))) = Bind t1 . mkAlt <$> rightHoistFetch t2
rightHoistFetch (Case v t alts) = (Case v <$> rightHoistFetch t) <*> mapM step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt <$> rightHoistFetch t
rightHoistFetch term = pure term

hoistFetch :: forall mf. MonadFresh Int mf => Int -> Abs -> Abs -> Term -> Int -> mf Term
hoistFetch n x1 x2 t offset =
    go (x2 <| x1 :| []) t
  where
    go :: List1 Abs -> Term -> mf Term
    go xs (Case (Var n) t alts)
      | x1 `elem` (toList xs !!! n) =
        let v = strengthen impossible $ Var n in
        Case v t <$> updateAlts xs alts

    go xs (Case (Var n) t alts `Bind` alt)
      | x1 `elem` (toList xs !!! n) = do
        let v = strengthen impossible $ Var n
        (Case v t <$> updateAlts xs alts) <&> (`Bind` strengthen (error $ "Cannot strengthen alt:\n" ++ prettyShow alt) alt)

    go (x1 :| xs) (t1 `Bind` LAltVar x2 t2) = do
      -- Swap indices 0 and 1
      t2' <- go (x1 <| x2 :| xs) (swap01' t2)
      pure $ strengthen impossible t1 `Bind` LAltVar x2 t2'


    -- go (x1 :| xs) (t1 `Bind` (splitLaltWithVars -> (mkAlt, t2, [x2]))) = do
    --   -- Swap indices 0 and 1
    --   t2' <- go (x1 <| x2 :| xs) (swap01' t2)
    --   pure $ strengthen impossible t1 `Bind` mkAlt t2'

    -- -- TODO
--    go xs1 (t1 `Bind` (splitLaltWithVars -> (mkAlt, t2, xs2))) = error "TODO need swap0n"

    go xs1 t = error $ "HOIST FETCH: " ++ prettyShow xs1 ++ " x1: " ++ prettyShow x1 ++ "\nTerm:\n" ++ prettyShow t



    updateAlts  :: List1 Abs -> [CAlt] -> mf [CAlt]
    updateAlts xs = mapM step
      where
      step (CAltTag tag t) =
        CAltTag tag <$> caseMaybe (prependFetchOnUse tag xs t)
          -- Strengthen alternatives which didn't prepend fetch
          (pure $ strengthen (error $ "DIDN'T PREPEND FETCH:\n" ++ prettyShow t) t)
          id
      step _ = __IMPOSSIBLE__

    prependFetchOnUse :: Tag -> List1 Abs -> Term -> Maybe (mf Term)
    prependFetchOnUse tag (x1 :| xs) (Bind t1 (splitLaltWithVars -> (mkAlt, t2, [x2])))
      | usesX2 (x1 :| xs) t1 = Just $ prependFetch tag (x1 :| xs) (t1 `Bind` mkAlt t2)
      | otherwise =
        caseMaybe (prependFetchOnUse tag (x1 :| xs) t1)
          ((fmap . fmap)
             -- Strengthen t1 if fetch is prepended to t2
            (\t2' -> strengthen impossible t1 `Bind` mkAlt t2')
            -- Swap indices 0 and 1
            (prependFetchOnUse tag (x1 <| x2 :| xs) (swap01' t2)))

          -- Keep t2 unchanged
          (Just . fmap (`Bind` mkAlt t2))


    prependFetchOnUse tag xs1 (Bind t1 (splitLaltWithVars -> (mkAlt, t2, xs2))) = error "TODO need swap0n"
    prependFetchOnUse tag xs t = boolToMaybe (usesX2 xs t) (prependFetch tag xs t)

    prependFetch :: Tag -> List1 Abs -> Term -> mf Term
    prependFetch tag xs t =  FetchOffset tag (n + length xs - 1) offset `bindVar` t

    usesX2 :: List1 Abs -> Term -> Bool
    usesX2 xs = \case
        App v vs        -> any (usesX2Val xs) (v : vs)
        Store _ v       -> usesX2Val xs v
        Unit v          -> usesX2Val xs v
        Update _ n v    -> isX2 xs n || usesX2Val xs v
        Error _         -> False
        FetchOpaqueOffset n _ -> isX2 xs n
        FetchOffset _ n _ -> isX2 xs n
        Bind{}          -> __IMPOSSIBLE__
        Case{}          -> __IMPOSSIBLE__
        Fetch'{}         -> __IMPOSSIBLE__
      where
        usesX2Val :: List1 Abs -> Val -> Bool
        usesX2Val xs = \case
          ConstantNode _ vs -> any (usesX2Val xs) vs
          VariableNode n vs -> isX2 xs n || any (usesX2Val xs) vs
          Var n             -> isX2 xs n
          _                 -> False

        isX2 xs n = caseMaybe (toList xs !!! n) False (== x2)

propagateConstant :: Term -> Term
propagateConstant (Case (Tag tag1) _ alts) = propagateConstant $ headWithDefault __IMPOSSIBLE__ (mapMaybe step alts)
  where
  step (CAltTag tag2 t) = boolToMaybe (tag2 == tag1) t
  step _                = Nothing
propagateConstant (Case v t alts) = Case v (propagateConstant t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (propagateConstant t)
propagateConstant (t1 `Bind` (splitLalt -> (mkAlt, t2))) = propagateConstant t1 `Bind` mkAlt (propagateConstant t2)
propagateConstant t = t

-- | Introduce registers for all operands. A precondition
--   the (unimplemented) common sub-expression elimination.
--
-- f #3 0 ; λ xs → <m>
-- >>>
-- unit #3 ; λ x →
-- f 0 1 ; λ xs → <m>
--
-- Note: currently this transformation is unused
-- introduceRegisters :: MonadFresh Int mf => Term -> mf Term
-- introduceRegisters term@((valsView -> Just (mkT, vs)) `Bind` (splitLalt -> (mkAlt, t2))) = do
--   term' <- useRegisters vs (\vs -> mkT vs `Bind` mkAlt t2)
--   if term' == term then do
--     t2' <- introduceRegisters t2
--     pure $ mkT vs `Bind` mkAlt t2'
--   else
--     introduceRegisters term'
--
-- TODO fix List1 a → [a]
-- introduceRegisters term@(Case v t1 alts `Bind` alt@(splitLalt -> (mkAlt, t2))) = do
--     term' <- useRegister v (\v -> Case v t1 alts `Bind` alt)
--     if term' == term then do
--       t1' <- introduceRegisters t1
--       alts' <- mapM go alts
--       t2' <- introduceRegisters t2
--       pure $ Case v t1' alts' `Bind` mkAlt t2'
--     else
--       introduceRegisters term'
--   where
--     go (splitCalt -> (mkAlt, t)) = mkAlt <$> introduceRegisters t
--
-- introduceRegisters (t1@(valsView -> Nothing) `Bind` (splitLalt -> (mkAlt, t2))) = do
--   introduceRegisters t2 <&> \t2' -> t1 `Bind` mkAlt t2'
--
-- introduceRegisters term@(Case v t alts) = do
--     term' <- useRegister v (\v -> Case v t alts)
--     if term' == term then do
--       t' <- introduceRegisters t
--       alts' <- mapM go alts
--       pure $ Case v t' alts'
--     else
--       introduceRegisters term'
--   where
--     go (splitCalt -> (mkAlt, t)) = mkAlt <$> introduceRegisters t
--
-- introduceRegisters (valsView -> Just (mkT, vs)) = useRegisters vs mkT
-- introduceRegisters t@(valsView -> Nothing) = pure t
-- introduceRegisters _ = __IMPOSSIBLE__ -- Redundant pattern but GHC can't figure it out
--
-- valsView :: Term -> Maybe (List1 Val -> Term, List1 Val)
-- valsView (UpdateTag tag n v) = Just (UpdateTag tag n . List1.head, v :| [])
-- valsView (App v1 vs)         = caseList vs Nothing $ \v2 vs -> Just (App v1 . toList, v2 :| vs)
-- valsView (Store loc v)       = Just (Store loc . List1.head, v :| [])
-- valsView (Unit (ConstantNode tag rc vs)) = Just (mkT, Tag tag <| vs) where
--   mkT (Var n :| v : vs) = Unit (VariableNode n rc $ v :| vs)
--   mkT _                 = __IMPOSSIBLE__
-- valsView Unit{}              = Nothing -- Otherwise it doesn't terminate
-- valsView Fetch{}             = Nothing
-- valsView Error{}             = Nothing
-- valsView Bind{}              = Nothing
-- valsView Case{}              = Nothing
-- valsView Update{}            = __IMPOSSIBLE__
--
-- useRegisters :: MonadFresh Int mf => List1 Val -> (List1 Val -> Term) -> mf Term
-- useRegisters vs mkT = f $ raise n $ mkT vs' where
--   ((f, n), vs') =
--     forAccumR (pure, 0) vs $ \(f, m) v ->
--       caseMaybe (mkRegister v)
--         ((f, m), v)
--         ((, Var $ m - n) . (, succ m) . (<=< f))
--
-- useRegister :: MonadFresh Int mf => Val -> (Val -> Term) -> mf Term
-- useRegister v mkT =  useRegisters (v :| []) (mkT . List1.head)
--
-- mkRegister :: MonadFresh Int mf => Val -> Maybe (Term -> mf Term)
-- mkRegister = \case
--   Var _ -> Nothing
--   Lit lit             -> Just $ bindVar (Unit $ Lit lit)
--   Tag tag             -> Just $ bindVar (Unit $ Tag tag)
--   ConstantNode tag rc vs -> Just $ \t -> do
--     alt <- laltVar t
--     let mkT (Var n :| v : vs) = Unit (VariableNode n rc $ v :| vs) `Bind` alt
--         mkT _                 = __IMPOSSIBLE__
--     useRegisters (Tag tag <| vs) mkT
--   VariableNode n rc vs -> Just $ \t -> do
--     alt <- laltVar t
--     let mkT vs' = Unit (VariableNode n rc vs') `Bind` alt
--     useRegisters vs mkT
--   v -> error $ "REG: " ++ prettyShow v

