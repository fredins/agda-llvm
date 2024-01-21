{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Compiler.Grin.GrinTransformations
  (module Compiler.Grin.GrinTransformations) where

import           Control.Applicative            (liftA2)
import           Control.Arrow                  (second)
import           Control.Monad                  (forM, (<=<), when)
import           Control.Monad.Reader           (MonadReader (local),
                                                 ReaderT (runReaderT), asks)
import           Control.Monad.State            (StateT (runStateT), modify)
import           Control.Monad.Trans.Maybe      (MaybeT (..), hoistMaybe)
import           Data.Foldable                  (find, fold, foldrM, toList)
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

import           Agda.Utils.Applicative         (forA)
import           Compiler.Grin.Grin             as G
import           Compiler.Grin.HeapPointsToType
import qualified Utils.List1                    as List1
import           Utils.Utils
import Utils.Foldable (foldFor)
import Data.List.Extra (list)
import Data.Tuple.Extra (secondM)

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

instance Semigroup TagInfo where
  t1 <> t2 = TagInfo $ on (<>) unTagInfo t1 t2

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
  TagInfo $ Map.fromList $ forMaybe (absCxt ^. lensAbsEnv) $ \(x, v) ->
    case v of
      VNode tag _ -> Just (x, Set.singleton tag)
      v@Union{} ->
        let tags = flip List1.mapMaybe (valueToList v) $ \case
                   VNode tag _ -> Just tag
                   _           -> Nothing in
        caseList tags Nothing $ \t ts -> Just (x, Set.fromList $ t : ts)
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



------------------------------------------------------------------------
-- * Simplifying transformations
------------------------------------------------------------------------

-- | Specialize and inline calls to eval (Boquist 1999, ch. 4.2.1).
evalInlining
  :: forall mf. MonadFresh Int mf
  => AbstractContext
  -> Map String Abs
  -> TagInfo
  -> GrinDefinition
  -> mf (TagInfo, Term)

evalInlining cxt returnVars tagInfo def = go tagInfo (reverse def.gr_args) def.gr_term
  where

  fetchTagSet :: Abs -> Maybe (Set Tag)
  fetchTagSet x = do
    vs <- valueToList <$> lookup x (unAbsEnv cxt.fEnv)
    locs <- forM vs \case Loc loc -> Just loc
                          _       -> Nothing
    foldFor locs \ loc ->
      fmap (step' . valueToList) $ lookup loc $ unAbsHeap cxt.fHeap
    where
    step' vs = Set.fromList $ List1.forMaybe vs \case
      VNode tag _ -> Just tag
      _            -> Nothing

  -- | Generate eval specialization
  --
  --   Example: 
  --   @
  --      eval n λ x9 → 
  --       
  --      >>>
  --      
  --      (fetch n λ x1 → 
  --       case 0 of
  --         C[]         → unit (C[])
  --         C_∷_ x2 x3  → unit (C_∷_ 1 0)
  --         FupTo x4 x5 → 
  --           upto x4 x5 ; λ x6 →
  --           (case 0 of
  --             C[] → update (n + 3) (C[])
  --             C_∷_ x7 x8 → update (n + 2 + 1 + 2) (C_∷_ 1 0)
  --           ) ; λ () → 
  --           unit 0
  --       ) λ x9 →    
  --   @
  --   tag info additions: 
  --     x9 → C[], C_∷_, FupTo
  --     x6 → C[], C_∷_
  generateEval
    :: forall mf. MonadFresh Int mf
    => TagInfo
    -> [Abs]
    -> Int
    -> mf (TagInfo, Set Tag, Term)
  generateEval tagInfo xs n = do
    x9 <- freshAbs

    let tagInfo' = tagInfoInsert x9 (Set.fromList $ List1.toList tags) tagInfo

    ((tagInfo'', bindingTags), alts) <- second List1.toList <$> forAccumM (tagInfo', mempty) tags 
      \ (tagInfo, bindingTags) tag -> do 
        ((tagInfo', bindingTags'), t) <- generateCaseBody tagInfo tag
        alt <- caltConstantNode tag t
        pure ((tagInfo <> tagInfo', bindingTags <> bindingTags'), alt)

    let eval = FetchOpaque n `Bind` LAltVar x9 (Case (Var 0) Unreachable alts)

    pure (tagInfo'', bindingTags, eval)
    where
    tags = list __IMPOSSIBLE__ (:|) 
         $ maybe __IMPOSSIBLE__ Set.toList 
         $ fetchTagSet (xs !! n)
 
    generateCaseBody :: forall mf. MonadFresh Int mf => TagInfo -> Tag -> mf ((TagInfo, Set Tag), Term)
    generateCaseBody tagInfo tag@CTag{} = 
      pure ((tagInfo, Set.singleton tag), Unit $ ConstantNode tag $ map Var $ downFrom tag.tArity)
    generateCaseBody tagInfo tag@FTag{} = do
      x6 <- freshAbs

      alts <- forM (Set.toList returnTags) \ returnTag -> 
        let update = Update returnTag (n + 2 + tag.tArity + returnTag.tArity) 
                   $ ConstantNode returnTag 
                   $ map Var 
                   $ downFrom returnTag.tArity 
        in caltConstantNode returnTag update

      let 
        t = app `Bind` LAltVar x6 (Case (Var 0) Unreachable alts `BindEmpty` Unit (Var 0))
        tagInfo' = tagInfoInsert x6 returnTags tagInfo

      pure ((tagInfo', returnTags), t)
      where 
      app = App (Def tag.tDef) (map Var $ downFrom tag.tArity)
      returnTags = fromMaybe __IMPOSSIBLE__ do 
        returnVar <- Map.lookup tag.tDef returnVars
        Map.lookup returnVar tagInfo.unTagInfo
    generateCaseBody _ PTag{} = __IMPOSSIBLE__


  go :: forall mf. MonadFresh Int mf
     => TagInfo
     -> [Abs]
     -> Term
     -> mf (TagInfo, Term)
  go tagInfo xs (App (Def "eval") [Var n] `Bind` LAltVar x t) = do
    (tagInfo', bindTags, eval) <- generateEval tagInfo xs n
    (tagInfo', t') <- go (tagInfoInsert x bindTags tagInfo') (x : xs) t
    pure (tagInfo', eval `Bind` LAltVar x t')
  go tagInfo xs (App (Def "eval") [Var n]) = do
    (tagInfo', _, eval) <- generateEval tagInfo xs n
    pure (tagInfo', eval)
  go tagInfo xs (Case v t alts) = do
    (tagInfo', t') <- go tagInfo xs t
    (tagInfo'', alts') <- mapAccumM step tagInfo' alts
    pure (tagInfo'', Case v t' alts')
    where
    step tagInfo (splitCaltWithVars -> (mkAlt, t, ys)) =
      second mkAlt <$> go tagInfo (reverse ys ++ xs) t
  go tagInfo xs (t1 `Bind` (splitLaltWithVars -> (mkAlt, t2, ys))) = do
    (tagInfo', t1') <- go tagInfo xs t1
    (tagInfo'', t2') <- go tagInfo' (reverse ys ++ xs) t2
    pure (tagInfo'', t1' `Bind` mkAlt t2')
  go tagInfo _ t = pure (tagInfo, t)



-- | Replace all node variables by explicit nodes (Boquist 1999, ch.4.2.4).
--
-- <t1> ; λ x₁ →
-- <t2>
-- >>>
-- <t1> ; λ tag x₂ x₃ →
-- <t2> [tag x₂ x₃ / x₁]
-- TODO update taginfo
vectorization :: forall mf. MonadFresh Int mf => TagInfo -> Term -> mf Term
vectorization tagInfo (t1 `Bind` LAltVar x1 t2)
  | Just tag <- singletonTag x1 tagInfo = do
    let arity = tagArity tag
        vs = map Var $ downFrom arity
        node = ConstantNode tag vs
        offset = arity
        rho = singletonS offset node `composeS` raiseS offset
    t2' <- vectorization tagInfo t2
    alt <- laltConstantNode tag (applySubst rho t2')
    pure (t1 `Bind` alt)


  | Just arity <- lookupMaxArity x1 tagInfo = do
    let vs = map Var $ downFrom arity
        node = VariableNode arity vs
        offset = succ arity
        rho = singletonS offset node `composeS` raiseS offset
    t2' <- vectorization tagInfo t2
    alt <- laltVariableNode arity (applySubst rho t2')
    pure (t1 `Bind` alt)

vectorization absCxt (t1 `Bind` (splitLalt -> (mkAlt, t2))) = do
  t1' <- vectorization absCxt t1
  t2' <- vectorization absCxt t2
  pure (t1' `Bind` mkAlt t2')

vectorization absCxt (Case v t alts) = do
  t' <- vectorization absCxt t
  alts' <- mapM step alts
  pure $ Case v t' alts'
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt <$> vectorization absCxt t

vectorization _ t = pure t

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

-- | (Boquist 1999, 4.2.4)
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
caseSimplification :: Term -> Term
caseSimplification (Case (ConstantNode tag vs) t alts) =
  Case (Tag tag) (caseSimplification t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = substPatternBinds vs (mkAlt (caseSimplification t))
caseSimplification (Case (VariableNode n vs) t alts) =
  Case (Var n) (caseSimplification t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = substPatternBinds vs (mkAlt (caseSimplification t))
caseSimplification (Case v t alts) = Case v (caseSimplification t) $ map step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt $ caseSimplification t
caseSimplification (t1 `Bind` (splitLalt -> (mkAlt, t2))) = caseSimplification t1 `Bind` mkAlt (caseSimplification t2)
caseSimplification term = term


fetchRest :: Maybe Tag -> Int -> [Abs] -> Term -> Term
fetchRest mtag n xs t = foldr (uncurry fetch) t (zip [2 ..] xs)
  where
  fetch offset x t = Fetch' mtag (n + offset - 2) (Just offset) `Bind` LAltVar x t

-- | Split fetch operations using offsets (Boquist 1999, ch. 4.2.7)
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
splitFetchOperations :: Term -> Term
splitFetchOperations (Fetch' mtag n Nothing `Bind` LAltConstantNode _ xs t) = fetchRest mtag n xs (splitFetchOperations t)
splitFetchOperations (Fetch' mtag n Nothing `Bind` LAltVariableNode x xs t) =
  Fetch' mtag n (Just 1) `Bind` LAltVar x (fetchRest mtag (n + 1) xs (splitFetchOperations t))
splitFetchOperations (t1 `Bind` (splitLalt -> (mkAlt, t2))) = splitFetchOperations t1 `Bind` mkAlt (splitFetchOperations t2)
splitFetchOperations (Case v t alts) = Case v (splitFetchOperations t) $ map step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (splitFetchOperations t)
splitFetchOperations term = term


-- FIXME ugly
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


-- (Boquist 1999, ch. 4.2.8)
-- TODO Implement the the missing patterns (see TODOs above)
rightHoistFetchOperations :: forall mf. MonadFresh Int mf => Term -> mf Term
rightHoistFetchOperations (FetchOpaqueOffset n 1      `Bind` LAltVar x1
                (FetchOpaqueOffset _ offset `Bind` LAltVar x2 t)) = do
  t' <- hoistFetch n x1 x2 t offset
  rightHoistFetchOperations (FetchOpaqueOffset n 1 `Bind` LAltVar x1 t')

rightHoistFetchOperations (Bind t1 (splitLalt-> (mkAlt, t2))) = Bind t1 . mkAlt <$> rightHoistFetchOperations t2
rightHoistFetchOperations (Case v t alts) = (Case v <$> rightHoistFetchOperations t) <*> mapM step alts
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt <$> rightHoistFetchOperations t
rightHoistFetchOperations term = pure term

------------------------------------------------------------------------
-- * Optimizing transformations
------------------------------------------------------------------------

-- | (Boquist 1999, ch. 4.3.2)
copyPropagation :: Term -> Term

-- | Left unit law
--
-- unit v ; λ x →
-- <m>
-- >>>
-- <m> [x / v]
copyPropagation (Unit (Var n) `Bind` LAltVar _ t) = strengthen impossible (applySubst (inplaceS 0 $ Var $ succ n) $ copyPropagation t)

-- -- unit (tag v₁ v₂) ; λ x →
-- -- 〈t 〉
-- -- >>>
-- -- 〈t 〉[(tag v₁ v₂) / 0]
-- TODO generalise to subsume the former clause
copyPropagation (Unit (ConstantNode tag vs) `Bind` LAltVar _ t) = t''
  where
  v = raise 1 (ConstantNode tag vs)
  t' = applySubst (inplaceS 0 v) $ copyPropagation t
  t'' = strengthen impossible t'

-- | Left unit law
--
-- unit (tag v₁ v₂) ; λ tag x₁ x₂ →
-- 〈t 〉
-- >>>
-- 〈t 〉[v₁ / 1, v₂ / 0]
copyPropagation (Unit (ConstantNode tag1 vs) `Bind` LAltConstantNode tag2 xs t)
  | tag1 == tag2 = t''
  | otherwise    = __IMPOSSIBLE__
  where
  numSubst = length xs
  vs' = raise numSubst $ reverse (take numSubst vs)
  -- Substitute away pattern variable usage
  t' = foldr applySubst (copyPropagation t) (zipWith inplaceS [0 ..] vs')
  -- Remove pattern variables by strengthening
  t'' = applySubst (strengthenS impossible numSubst) t'


-- | Right unit law
copyPropagation (t `Bind` LAltVar _ (Unit (Var 0))) = t

copyPropagation (Bind t1 (splitLalt -> (mkAlt, t2))) = Bind (copyPropagation t1) (mkAlt $ copyPropagation t2)
copyPropagation (Case n t alts) = Case n (copyPropagation t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (copyPropagation t)
copyPropagation t = t


-- | (Boquist 1999, ch. 4.3.12)
-- TODO also replace variables by tags using tag info
constantPropagation :: Term -> Term
constantPropagation (Case (Tag tag1) _ alts) = constantPropagation $ headWithDefault __IMPOSSIBLE__ (mapMaybe step alts)
  where
  step (CAltTag tag2 t) = boolToMaybe (tag2 == tag1) t
  step _                = Nothing
constantPropagation (Case v t alts) = Case v (constantPropagation t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (constantPropagation t)
constantPropagation (t1 `Bind` (splitLalt -> (mkAlt, t2))) = constantPropagation t1 `Bind` mkAlt (constantPropagation t2)
constantPropagation t = t

------------------------------------------------------------------------
-- * Miscellaneous transformations
------------------------------------------------------------------------
  
-- | Normalise the GRIN expression by making the expression right-skewed (Boquist 1999, ch. 4.4.1).
bindNormalisation :: Term -> Term
bindNormalisation (Bind (Bind t (splitLaltWithVars -> (mkAlt1, t1, xs))) (splitLalt -> (mkAlt2, t2))) =
  bindNormalisation $ Bind t $ mkAlt1 $ Bind t1 $ raise (length xs) (mkAlt2 t2)
bindNormalisation (Bind t1 (splitLalt -> (mkAlt, t2))) = bindNormalisation t1 `Bind` mkAlt (bindNormalisation t2)
bindNormalisation (Case n t alts) = Case n (bindNormalisation t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (bindNormalisation t)
bindNormalisation t = t













-- NOT USED RIGHT NOW MAYBE IN THE FUTURE
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

