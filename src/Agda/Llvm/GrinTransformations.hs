{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Agda.Llvm.GrinTransformations (module Agda.Llvm.GrinTransformations) where


import           Control.Applicative                (liftA2)
import           Control.Monad                      (ap, forM, replicateM,
                                                     (<=<))
import           Control.Monad.Reader               (MonadReader (local),
                                                     ReaderT (runReaderT), asks)
import           Control.Monad.State                (StateT (runStateT),
                                                     evalStateT, gets, modify)
import           Data.Foldable                      (find, foldrM, toList)
import           Data.Function                      (on)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set

import           Agda.Compiler.Backend              hiding (Prim)
import           Agda.Llvm.Grin
import           Agda.Llvm.HeapPointsTo
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.SizedTypes.Utils (trace)
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Applicative             (forA)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1                   (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1                   as List1
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                   (forMaybeM)
import           Control.Monad.Trans.Maybe          (MaybeT (..), hoistMaybe)

newtype TagInfo = TagInfo{unTagInfo :: Map Abs (Set Tag)}

lensTagInfo :: Lens' TagInfo (Map Abs (Set Tag))
lensTagInfo f tagInfo = TagInfo <$> f tagInfo.unTagInfo

tagInfoInsert :: Abs -> Set Tag -> TagInfo -> TagInfo
tagInfoInsert = over lensTagInfo .: Map.insert

lookupMaxArity :: Abs -> TagInfo -> Maybe Int
lookupMaxArity abs tagInfo =
  maximum . map tArity . Set.toList <$> Map.lookup abs tagInfo.unTagInfo

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
      t' <- localAbs x (go t)
      pure $ eval `Bind` LAltVar x t'
    App (Def "eval") [Var n] -> do
      fst <$> generateEval n
    Case i t alts ->
      liftA2 (Case i) (go t) $ forM alts $ \(splitCaltAbss -> (mkAlt, t, xs)) -> mkAlt <$> localAbss xs (go t)
    Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs)) ->
      liftA2 Bind (go t1) (mkAlt <$> localAbss xs (go t2))
    term -> pure term

  generateEval :: Int -> E mf (Term, Set Tag)
  generateEval n = do
    x <- deBruijnLookup n
    case Set.toList (fetchTagSet x) of
      []     -> __IMPOSSIBLE__
      [tag]  -> genLambda tag
      (t:ts) -> genCase (t :| ts)
    where
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
          bindConstantNode t1 t2 = Bind t1 <$> laltConstantNode tag t2
          bindVar' t1 t2 = t1 `Bind` LAltVar x t2
          eval =
            FetchNode n                    `bindConstantNode`
            genBody tag                    `bindVar'`
            Update Nothing (n + 2) (Var 0) `BindEmpty`
            Unit (Var 0)

      eval <&> (, returnTags)
      where
      returnTags = mkReturnTags [tag]

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
          FetchNode n                       `bindVarR'`
          Case (Var 0) unreachable <$> alts `bindVarL'`
          Update Nothing (n + 2) (Var 0)    `BindEmpty`
          Unit (Var 0)

      eval <&> (, returnTags)
      where
      caseTags = Set.fromList (toList tags)
      returnTags = mkReturnTags tags
      alts = mapM (caltConstantNode <*> genBody) (toList tags)

    genBody FTag{tDef, tArity} = App (Def tDef) $ map Var $ downFrom tArity
    genBody tag@CTag{tArity = 0} = Unit (Tag tag)
    genBody tag@CTag{tArity = downFrom -> v : vs} = Unit (ConstantNode tag (List1.map Var (v :| vs)))
    genBody _ = __IMPOSSIBLE__

  fetchTagSet :: Abs -> Set Tag
  fetchTagSet = maybe __IMPOSSIBLE__ (foldMap filterTags) . (mapM heapLookup <=< fetchLocations)

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

  localAbs :: Abs -> E mf a -> E mf a
  localAbs abs = local (abs :)

  localAbss :: [Abs] -> E mf a -> E mf a
  localAbss = foldl (.: localAbs) id

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
leftUnitLaw (Unit (Var n) `Bind` LAltVar _ t) =
    applySubst rho (leftUnitLaw t)
  where
    rho = liftS 1 (strengthenS impossible 1 `composeS` inplaceS 0 (Var n))

-- unit (tag v₁ v₂) ; λ tag x₁ x₂ →
-- 〈t 〉
-- >>>
-- 〈t 〉[v₁ / 1, v₂ / 0]
leftUnitLaw (Unit (ConstantNode tag1 vs) `Bind` LAltConstantNode tag2 xs t)
  | tag1 == tag2 =
    let
      arity = tagArity tag1
      vs' = List1.map (raise arity) $ List1.reverse vs
      rho =
        strengthenS impossible arity `composeS`
        foldr composeS IdS (List1.zipWith inplaceS (0 :| [1 .. arity - 1]) vs')
    in
    applySubst rho $ leftUnitLaw t

  | otherwise = __IMPOSSIBLE__

leftUnitLaw term = case term of
  Bind t alt    -> Bind (leftUnitLaw t) (removeUnitBindLalt alt)
  Case n t alts -> Case n (leftUnitLaw t) (map removeUnitBindCalt alts)
  _             -> term

removeUnitBindLalt :: LAlt -> LAlt
removeUnitBindLalt (LAltVar abs t) = LAltVar abs $ leftUnitLaw t
removeUnitBindLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ leftUnitLaw t
removeUnitBindLalt (LAltVariableNode x abss t) = LAltVariableNode x abss $ leftUnitLaw t
removeUnitBindLalt (LAltEmpty t) = LAltEmpty $ leftUnitLaw t

removeUnitBindCalt :: CAlt -> CAlt
removeUnitBindCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ leftUnitLaw t
removeUnitBindCalt (CAltLit lit t) = CAltLit lit $ leftUnitLaw t
removeUnitBindCalt (CAltTag tag t) = CAltTag tag $ leftUnitLaw t

-- | Normalise the GRIN expression by making the expression right-skewed.
normalise :: Term -> Term
normalise (Bind t1 alt1)
  | Bind t2 alt2 <- normalise t1 =
    let alt1' = raise (countBinders alt2) $ normaliseLalt alt1
        (mkAlt2, t_alt2) = splitLalt alt2 in
    normalise $ Bind t2 (mkAlt2 (Bind t_alt2 alt1'))

normalise term = case term of
  Bind t alt    -> Bind (normalise t) (normaliseLalt alt)
  Case n t alts -> Case n (normalise t) (map normaliseCalt alts)
  _             -> term

normaliseLalt :: LAlt -> LAlt
normaliseLalt (LAltVar abs t)       = LAltVar abs $ normalise t
normaliseLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ normalise t
normaliseLalt (LAltEmpty t)         = LAltEmpty $ normalise t


normaliseCalt :: CAlt -> CAlt
normaliseCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ normalise t
normaliseCalt (CAltLit lit t)       = CAltLit lit $ normalise t

countBinders :: LAlt -> Int
countBinders (LAltConstantNode _ abss _) = length abss
countBinders LAltVar{}                   = 1
countBinders LAltEmpty{}                 = 0

data UpdateCxt = UpdateCxt
  { absCxt :: AbstractContext
  , vars   :: [Abs]
  }

initUpdateCxt :: AbstractContext -> GrinDefinition -> UpdateCxt
initUpdateCxt absCxt def = UpdateCxt{absCxt = absCxt, vars = reverse def.gr_args}

specializeUpdate :: forall mf. MonadFresh Int mf => AbstractContext -> GrinDefinition -> mf GrinDefinition
specializeUpdate _absCxt def = do
  term <- runReaderT (go def.gr_term) (initUpdateCxt _absCxt def)
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
        CAltConstantNode tag xs t ->
          let update = raise tag.tArity $ mkUpdate tag in
          CAltConstantNode tag xs $ update `BindEmpty` t
        _ -> __IMPOSSIBLE__

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
  go term@(Fetch n1 offset      `Bind` LAltVar x1
          (Case v1 t alts       `Bind` LAltVar x2
          (Update Nothing n2 v2 `BindEmpty`
           Unit v2')))
    | v2' == v2 = do
    x <- asks $ fromMaybe __IMPOSSIBLE__ . (!!! n1) . vars
    tags <- maybe __IMPOSSIBLE__ toList <$> runMaybeT (fetchTagSet =<< fetchLocations x)
    alts <- mapM (caltConstantNode <*> genBody) tags
    -- Rerun the transformation so it matches the case pattern
    go (Fetch n1 offset      `Bind` LAltVar x1
       (Case v1 t alts       `Bind` LAltVar x2
       (Update Nothing n2 v2 `BindEmpty`
        Case v2 unreachable alts)))



    --(term `bindVar` Case (Var 0) unreachable alts)
    where
      genBody FTag{tDef, tArity} = App (Def tDef) $ map Var $ downFrom tArity
      genBody tag@CTag{tArity = 0} = Unit (Tag tag)
      genBody tag@CTag{tArity = downFrom -> v : vs} = Unit (ConstantNode tag (List1.map Var (v :| vs)))
      genBody _ = __IMPOSSIBLE__

      fetchTagSet :: List1 Loc -> MaybeT (ReaderT UpdateCxt mf) (Set Tag)
      fetchTagSet locs = do
        heap <- asks ((^. lensAbsHeap) . absCxt)
        hoistMaybe (foldMap filterTags <$> mapM (`lookup` heap) locs)

      fetchLocations :: Abs -> MaybeT (ReaderT UpdateCxt mf) (List1 Loc)
      fetchLocations x = do
        env <- asks ((^. lensAbsEnv) . absCxt)
        hoistMaybe (mapM isLoc . valueToList =<< lookup x env)
        where
        isLoc = \case
          Loc loc -> Just loc
          _       -> Nothing

      filterTags :: Value -> Set Tag
      filterTags (valueToList -> vs) =
        Set.fromList $ forMaybe (toList vs) $
          \case
            VNode tag _ -> Just tag
            _           -> Nothing

  go (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs))) =
    liftA2 (\t1 t2 -> t1 `Bind` mkAlt t2) (go t1) (varLocals xs (go t2))
  go (Case n t alts) = do
    alts' <- mapM (\(splitCaltAbss -> (mkAlt, t, xs)) -> mkAlt <$> varLocals xs (go t)) alts
    t' <- go t
    pure (Case n t' alts')
  go (Update mtag n1 n2)
    | Nothing <- mtag = __IMPOSSIBLE__
    | otherwise = pure (Update mtag n1 n2)
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
-- TODO use splitLaltWithAbss
caseUpdateView :: Term -> Maybe (Tag -> Term, Term -> Term, Term)
caseUpdateView (Update Nothing n t1 `BindEmpty` t) = go IdS id t where
  go :: Substitution' Val
     -> (Term -> Term)
     -> Term
     -> Maybe (Tag -> Term,Term -> Term, Term)
  go rho m (Case t2 t3 alts)
    | t1' <- applySubst rho t1
    , t1' == t2 =
      let n' = case lookupS rho n of
                 Var n -> n
                 _     -> __IMPOSSIBLE__ in
      Just (\tag -> Update (Just tag) n' t1', m , Case t2 t3 alts)
    | otherwise = Nothing
  go rho m (Bind t alt) = goLalt rho (m . Bind t) alt
  go _ _ _ = Nothing

  goLalt :: Substitution' Val
        -> (LAlt -> Term)
        -> LAlt
        -> Maybe (Tag -> Term, Term -> Term, Term)
  goLalt rho m (LAltVar abs t) = go (raiseS 1 `composeS` rho) (m . LAltVar abs) t
  goLalt rho m (LAltConstantNode tag abss t) =
    go (raiseS tag.tArity `composeS` rho) (m . LAltConstantNode tag abss) t
  goLalt rho m (LAltEmpty t) = go rho (m . LAltEmpty) t


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
    let
      arity = tagArity tag
      vs = caseList (map Var $ downFrom arity) __IMPOSSIBLE__ (:|)
      rho =
        inplaceS 0 (ConstantNode tag vs) `composeS`
        raiseFromS 1 (arity - 1)
    t2' <- applySubst rho <$> vectorize tagInfo t2
    Bind t1 <$> laltConstantNode tag t2'
  | Just arity <- lookupMaxArity x1 tagInfo = do
    let vs = caseList (map Var $ downFrom arity) __IMPOSSIBLE__ (:|)
        rho =
          inplaceS 0 (VariableNode arity vs) `composeS`
          raiseFromS 1 arity
    t2' <- applySubst rho <$> vectorize tagInfo t2
    Bind t1 <$> laltVariableNode arity t2'

vectorize absCxt (t1 `Bind` alt) =
  let (mkAlt, t2) = splitLalt alt in do
  t1' <- vectorize absCxt t1
  t2' <- vectorize absCxt t2
  pure $ t1' `Bind` mkAlt t2'

vectorize absCxt (Case v t alts) = do
  t' <- vectorize absCxt t
  alts' <- forM alts $ \alt ->
    let (mkAlt, t) = splitCalt alt in
    mkAlt <$> vectorize absCxt t
  pure $ Case v t' alts'

vectorize _ t = pure t

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
simplifyCase (Case v t alts)
  | ConstantNode tag vs <- v = Case (Tag tag) t' $ map (mkAlt $ toList vs) alts
  | VariableNode n vs <- v = Case (Var n) t' $ map (mkAlt $ toList vs) alts
  | otherwise =
    let go = (\(mkAlt, t) -> mkAlt $ simplifyCase t) . splitCalt in
    Case v t' $ map go alts
  where
    t' = simplifyCase t

    mkAlt vs (CAltConstantNode tag xs t)
      | arity <- tagArity tag
      , arity > 0 =
        let
          vs' = raise arity $ reverse $ take arity vs

          -- Substitute pattern variables by node variables
          rho =
            strengthenS impossible arity `composeS`
            foldr composeS IdS (zipWith inplaceS [0 .. arity - 1] vs')
         in
         CAltTag tag $ applySubst rho $ simplifyCase t
      | otherwise = CAltTag tag (simplifyCase t)
    mkAlt _ (splitCalt -> (mkAlt, t)) = mkAlt (simplifyCase t)

simplifyCase (t1 `Bind` alt) =
    simplifyCase t1 `Bind` mkAlt (simplifyCase t2)
  where
    (mkAlt, t2) = splitLalt alt

simplifyCase term = term

-- | Split fetch operations using offsets
-- <t1>
-- fetch p ; λ tag x₁ x₂ →
-- <t2>
-- >>>
-- <t1>
-- fetch p [0]; λ tag →
-- fetch p [1]; λ x₁ →
-- fetch p [2]; λ x₂ →
-- <t2>
-- TODO returning fetch [Boquist 1999, p. 105]
splitFetch :: Term -> Term
splitFetch (FetchNode n `Bind` alt)
  | LAltVariableNode x xs t <- alt =
    let mkFetch x m t = FetchOffset (n + m) m `Bind` LAltVar x t in
    FetchOffset n 0 `Bind` LAltVar x
    (mkFetchs xs (splitFetch t) mkFetch)
  | LAltConstantNode tag xs t <- alt =
    let mkFetch x m t = FetchOffset (n + m - 1) m `Bind` LAltVar x t in
    mkFetchs xs (splitFetch t) mkFetch
  where
    mkFetchs xs t f = foldr (uncurry f) t $ zip xs [1 ..]

splitFetch (t1 `Bind` alt) =
  splitFetch t1 `Bind` mkAlt (splitFetch t2)
  where
    (mkAlt, t2) = splitLalt alt

splitFetch (Case v t alts) =
    Case v (splitFetch t) $ map go alts
  where
    go = (\(mkAlt, t) -> mkAlt $ splitFetch t) . splitCalt

splitFetch term = term

-- TODO
-- • Maybe leave fetch operations that are used by all alternatives and all uses the
--   small layout.
-- • Annotate fetch with tag so it is known whether small layout or big layout should
--   be used.
-- • Implement the the missing patterns (see TODOs below)
rightHoistFetch :: forall mf. MonadFresh Int mf => Term -> mf Term
rightHoistFetch term@(FetchOffset n1 0       `Bind` LAltVar x1
                     (FetchOffset n1' offset `Bind` LAltVar x2 t2))
  | n1' - 1 == n1 = do
    t3 <- hoistFetch x1 n1' x2 t2 offset
    rightHoistFetch $ FetchOffset n1 0 `Bind` LAltVar x1 t3

rightHoistFetch (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs))) =
  Bind t1 . mkAlt <$> rightHoistFetch t2

rightHoistFetch (Case v t alts) =
   (Case v <$> rightHoistFetch t) <*> mapM go alts
  where
    go (splitCalt -> (mkAlt, t)) = mkAlt <$> rightHoistFetch t

rightHoistFetch term = pure term

hoistFetch :: forall mf. MonadFresh Int mf => Abs -> Int -> Abs -> Term -> Int -> mf Term
hoistFetch x1 n x2 t offset =
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
        (Case v t <$> updateAlts xs alts) <&> (`Bind` strengthen impossible alt)

    go (x1 :| xs) (t1 `Bind` (splitLaltWithAbss -> (mkAlt, t2, [x2]))) = do
      -- Swap indices 0 and 1
      t2' <- go (x1 <| x2 :| xs) (swap01' t2)
      pure $ strengthen impossible t1 `Bind` mkAlt t2'


    -- -- TODO
    -- go xs1 (t1 `Bind` (splitLaltWithAbss -> (mkAlt, t2, xs2))) = error "TODO need swap0n"

    go xs1 t = error $ "HOIST FETCH: " ++ prettyShow xs1 ++ "\nTerm:\n" ++ prettyShow t

    updateAlts  :: List1 Abs -> [CAlt] -> mf [CAlt]
    updateAlts xs alts =
      forM alts $ \(splitCalt -> (mkAlt, t)) -> mkAlt <$>
        caseMaybe (prependFetchOnUse xs t)
          -- Strengthen alternatives which didn't prepend fetch
          (pure $ strengthen impossible t)
          id

    prependFetchOnUse :: List1 Abs -> Term -> Maybe (mf Term)
    prependFetchOnUse (x1 :| xs) (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, [x2])))
      | usesX2 (x1 :| xs) t1 =
        Just $ prependFetch (x1 :| xs) (t1 `Bind` mkAlt t2)
      | otherwise =
        ifJust (prependFetchOnUse (x1 :| xs) t1)
          -- Keep t2 unchanged
          (Just . fmap (`Bind` mkAlt t2)) $

          (fmap . fmap)
             -- Strengthen t1 if fetch is prepended to t2
            (\t2' -> strengthen impossible t1 `Bind` mkAlt t2')
            -- Swap indices 0 and 1
            (prependFetchOnUse (x1 <| x2 :| xs) (swap01' t2))

    prependFetchOnUse xs1 (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs2))) = error "TODO need swap0n"
    prependFetchOnUse xs t = boolToMaybe (usesX2 xs t) (prependFetch xs t)

    prependFetch :: List1 Abs -> Term -> mf Term
    prependFetch xs t =  FetchOffset (n + length xs - 2) offset `bindVar` t

    usesX2 :: List1 Abs -> Term -> Bool
    usesX2 xs = \case
        App v vs        -> any (usesX2Val xs) (v : vs)
        Store _ v       -> usesX2Val xs v
        Unit v          -> usesX2Val xs v
        Update _ n v    -> isX2 xs n || usesX2Val xs v
        Error _         -> False
        FetchOffset n _ -> isX2 xs n
        Bind{}          -> __IMPOSSIBLE__
        Case{}          -> __IMPOSSIBLE__
        Fetch{}         -> __IMPOSSIBLE__
      where
        usesX2Val :: List1 Abs -> Val -> Bool
        usesX2Val xs = \case
          ConstantNode _ vs -> any (usesX2Val xs) vs
          VariableNode n vs -> isX2 xs n || any (usesX2Val xs) vs
          Var n             -> isX2 xs n
          _                 -> False

        isX2 xs n = caseMaybe (toList xs !!! n) False (== x2)


-- | Introduce registers for all operands. A precondition
--   the (unimplemented) common sub-expression elimination.
--
-- f #3 0 ; λ xs → <m>
-- >>>
-- unit #3 ; λ x →
-- f 0 1 ; λ xs → <m>
--
-- Note: currently this transformation is unused
introduceRegisters :: MonadFresh Int mf => Term -> mf Term
introduceRegisters term@((valsView -> Just (mkT, vs)) `Bind` (splitLalt -> (mkAlt, t2))) = do
  term' <- useRegisters vs (\vs -> mkT vs `Bind` mkAlt t2)
  if term' == term then do
    t2' <- introduceRegisters t2
    pure $ mkT vs `Bind` mkAlt t2'
  else
    introduceRegisters term'

introduceRegisters term@(Case v t1 alts `Bind` alt@(splitLalt -> (mkAlt, t2))) = do
    term' <- useRegister v (\v -> Case v t1 alts `Bind` alt)
    if term' == term then do
      t1' <- introduceRegisters t1
      alts' <- mapM go alts
      t2' <- introduceRegisters t2
      pure $ Case v t1' alts' `Bind` mkAlt t2'
    else
      introduceRegisters term'
  where
    go (splitCalt -> (mkAlt, t)) = mkAlt <$> introduceRegisters t

introduceRegisters (t1@(valsView -> Nothing) `Bind` (splitLalt -> (mkAlt, t2))) = do
  introduceRegisters t2 <&> \t2' -> t1 `Bind` mkAlt t2'

introduceRegisters term@(Case v t alts) = do
    term' <- useRegister v (\v -> Case v t alts)
    if term' == term then do
      t' <- introduceRegisters t
      alts' <- mapM go alts
      pure $ Case v t' alts'
    else
      introduceRegisters term'
  where
    go (splitCalt -> (mkAlt, t)) = mkAlt <$> introduceRegisters t

introduceRegisters (valsView -> Just (mkT, vs)) = useRegisters vs mkT
introduceRegisters t@(valsView -> Nothing) = pure t
introduceRegisters _ = __IMPOSSIBLE__ -- Redundant pattern but GHC can't figure it out

valsView :: Term -> Maybe (List1 Val -> Term, List1 Val)
valsView (UpdateTag tag n v) = Just (UpdateTag tag n . List1.head, v :| [])
valsView (App v1 vs)         = caseList vs Nothing $ \v2 vs -> Just (App v1 . toList, v2 :| vs)
valsView (Store loc v)       = Just (Store loc . List1.head, v :| [])
valsView (Unit (ConstantNode tag vs)) = Just (mkT, Tag tag <| vs) where
  mkT (Var n :| v : vs) = Unit (VariableNode n $ v :| vs)
  mkT _                 = __IMPOSSIBLE__
valsView Unit{}              = Nothing -- Otherwise it doesn't terminate
valsView Fetch{}             = Nothing
valsView Error{}             = Nothing
valsView Bind{}              = Nothing
valsView Case{}              = Nothing
valsView Update{}            = __IMPOSSIBLE__

useRegisters :: MonadFresh Int mf => List1 Val -> (List1 Val -> Term) -> mf Term
useRegisters vs mkT = f $ raise n $ mkT vs' where
  ((f, n), vs') =
    forAccumR (pure, 0) vs $ \(f, m) v ->
      caseMaybe (mkRegister v)
        ((f, m), v)
        ((, Var $ m - n) . (, succ m) . (<=< f))

useRegister :: MonadFresh Int mf => Val -> (Val -> Term) -> mf Term
useRegister v mkT =  useRegisters (v :| []) (mkT . List1.head)

mkRegister :: MonadFresh Int mf => Val -> Maybe (Term -> mf Term)
mkRegister = \case
  Var _ -> Nothing
  Lit lit             -> Just $ bindVar (Unit $ Lit lit)
  Tag tag             -> Just $ bindVar (Unit $ Tag tag)
  ConstantNode tag vs -> Just $ \t -> do
    alt <- laltVar t
    let mkT (Var n :| v : vs) = Unit (VariableNode n $ v :| vs) `Bind` alt
        mkT _                 = __IMPOSSIBLE__
    useRegisters (Tag tag <| vs) mkT
  VariableNode n vs -> Just $ \t -> do
    alt <- laltVar t
    let mkT vs' = Unit (VariableNode n vs') `Bind` alt
    useRegisters vs mkT
  v -> error $ "REG: " ++ prettyShow v

{-

DownFrom.sum r17 x16 =
  fetch 0 [1] ; λ x44 →
  DownFrom.downFrom 0 ; λ x63 x64 x65 →
  case 2 of
    CDownFrom.List.[] →
      updateCDownFrom.List.[] 4 (2 1 0) ; λ () →
      unit (Cnat #0)
    CDownFrom.List._∷_ →
      updateCDownFrom.List._∷_ 4 (2 1 0) ; λ () →
      storel10 (FDownFrom.sum 0) ; λ x11 →
      Agda.Builtin.Nat._+_ 0 2




drop x₀ = 
  fetch 0 [0] ; λ x₁ → 
  case 0 of
    0 → 
      fetch 1 [1] ; x₂ →
      case 0 of
        _∷_ → 
          fetch 2 [2] ; λ x₃ → 
          fetch 3 [3] ; λ x₄ → 
          drop x₃ ; λ () → 
          drop x₄ ; λ () → 
          free 4
        [] → 
          free 2
        downFrom → 
          fetch 2 [2] ; λ x₅ → 
          drop x₅ ; λ () → 
          free 4
        ...
    _ → 
      decref 0
-}







