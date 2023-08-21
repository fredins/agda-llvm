{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Agda.Llvm.GrinTransformations (module Agda.Llvm.GrinTransformations) where


import           Control.Applicative          (liftA2)
import           Control.Monad                (forM, replicateM, (<=<))
import           Control.Monad.Reader         (MonadReader (local),
                                               ReaderT (runReaderT), asks)
import           Control.Monad.State          (StateT (runStateT), evalStateT,
                                               gets, modify)
import           Data.Foldable                (find, foldrM, toList)
import           Data.Function                (on)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Agda.Compiler.Backend        hiding (Prim)
import           Agda.Llvm.Grin
import           Agda.Llvm.HeapPointsTo
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.Maybe

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
-- eval 3 ; λ x14 →
-- >>>
-- (fetch 3 ; λ x42 →
--  (case 0 of
--     FPrim.sub x38 x39 → Prim.sub 1 0
--     Cnat x40 → unit 0
--  ) ; λ x41 →
--  update 2 0 ; λ () →
--  unit 0
-- ) ; λ x14 →
inlineEval :: forall mf. MonadFresh Int mf
           => [GrinDefinition]
           -> AbstractContext
           -> TagInfo
           -> mf ([GrinDefinition], TagInfo)
inlineEval defs absCxt tagInfo =
    flip runStateT tagInfo $ forM defs $ \def -> do
      t <- runReaderT (go def.gr_term) (def.gr_args ++ returnAbs)
      pure def{gr_term = t}
  where
    returnAbs = mapMaybe gr_return defs

    localAbs :: Abs -> E mf a -> E mf a
    localAbs abs = local (abs :)

    localAbss :: [Abs] -> E mf a -> E mf a
    localAbss = foldl (.: localAbs) id

    heapLookup :: Loc -> Maybe Value
    heapLookup loc = lookup loc $ unAbsHeap absCxt.fHeap

    envLookup :: Abs -> Maybe Value
    envLookup abs = lookup abs $ unAbsEnv absCxt.fEnv

    -- ugly
    mkTagSet :: Abs -> Set Tag
    mkTagSet x = flip foldMap (collectTags x) $ \case
      FTag{tDef} ->
        let xs =
              forMaybe defs $ \def ->
                boolToMaybe (tDef == def.gr_name) =<< def.gr_return in
        caseList xs (Set.singleton natTag) $ \x _ ->
          fromMaybe __IMPOSSIBLE__ $ valueToTags =<< envLookup x
      PTag{} -> error "TODO"
      ctag -> Set.singleton ctag


    go :: Term -> E mf Term
    go term = case term of
      App (Def "eval") [Var n] `Bind` LAltVar x1 t1 -> do
          x2 <- deBruijnLookup n
          let tagSet = mkTagSet x2
          modify (tagInfoInsert x1 tagSet)
          t2 <- genEval n x2 tagSet
          t1' <- localAbs x1 $ go t1
          pure $ t2 `Bind` LAltVar x1 t1'
      App (Def "eval") [Var n] -> do
        x <- deBruijnLookup n
        genEval n x $ mkTagSet x
      Case i t alts            -> liftA2 (Case i) (go t) (mapM goCalt alts)
      Bind t alt               -> liftA2 Bind (go t) (goLalt alt)
      _ -> pure term

    goLalt :: LAlt -> E mf LAlt
    goLalt (LAltEmpty t)         = LAltEmpty <$> go t
    goLalt (LAltVar abs t)       = LAltVar abs <$> localAbs abs (go t) -- FIXME need to assign this abs
    goLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss <$> localAbss abss (go t)

    goCalt :: CAlt -> E mf CAlt
    goCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss <$> localAbss abss (go t)
    goCalt (CAltLit lit t)       = CAltLit lit <$> go t


    genEval :: Int -> Abs -> Set Tag -> E mf Term
    genEval n x1 tagSet
      | tag :| [] <- collectTags x1 = do

        x2 <- freshAbs
        modify (tagInfoInsert x2 tagSet)

        body <- genBody tag

        let infixr 2 `bindConstantNode`, `bindVar'`
            bindConstantNode t1 t2 = Bind t1 <$> laltConstantNode tag t2
            bindVar' t1 t2 = t1 `Bind` LAltVar x2 t2 in

          -- Generated term
          FetchNode n                    `bindConstantNode`
          body                           `bindVar'`
          Update Nothing (n + 2) (Var 0) `BindEmpty`
          Unit (Var 0)

      | otherwise = do

        let tags = collectTags x1
        x2 <- freshAbs
        modify (tagInfoInsert x2 $ Set.fromList $ toList tags)

        x3 <- freshAbs
        modify (tagInfoInsert x3 tagSet)

        let infixr 2 `bindVarL'`, `bindVarR'`
            bindVarR' t1 t2 = t2 <&> \t2' ->  t1 `Bind` LAltVar x2 t2'
            bindVarL' t1 t2 = t1 <&> \t1' ->  t1' `Bind` LAltVar x3 t2
            caseOf =
              Case (Var 0) unreachable . toList <$>
              forM tags (\tag -> caltConstantNode tag =<< genBody tag) in

          -- Generated term
          FetchNode n                    `bindVarR'`
          caseOf                         `bindVarL'`
          Update Nothing (n + 2) (Var 0) `BindEmpty`
          Unit (Var 0)

      where

        genBody :: Tag -> E mf Term
        genBody FTag{tDef, tArity}
          | isJust $ find ((tDef==). gr_name) defs =
            pure $ App (Def tDef) $ map Var $ downFrom tArity
          -- Wrap primitive values
          | otherwise =
            let prim = Prim $ strPrim tDef
                ts = (map Var $ downFrom tArity) in
            App prim ts `bindVar`
            Unit (ConstantNode natTag $ Var 0 :| [])

        genBody tag@CTag{tArity}
          | tArity == 0 = pure $ Unit $ Tag tag
          | otherwise =
            let vs = caseList (map Var $ downFrom tArity) __IMPOSSIBLE__ (:|) in
            pure $ Unit $ ConstantNode tag vs

        genBody PTag{} = error "TODO"


        strPrim :: String -> TPrim
        -- strPrim "Prim.add" = PAdd64
        strPrim "Prim.add" = PAdd
        -- strPrim "Prim.sub" = PSub64
        strPrim "Prim.sub" = PSub
        strPrim p          = error $ "TODO primStr " ++ show p

    collectTags :: Abs -> List1 Tag
    collectTags abs =
        go $ fromMaybe __IMPOSSIBLE__ $ envLookup abs
      where
        go (Loc loc)     = go $ fromMaybe __IMPOSSIBLE__ $ heapLookup loc
        go (Union v1 v2) = List1.nub $ on (<>) go v1 v2
        go (VNode tag _) =
          List1.singleton tag
        go _             = __IMPOSSIBLE__

    deBruijnLookup :: Int -> E mf Abs
    deBruijnLookup n = asks $ fromMaybe __IMPOSSIBLE__ . (!!! n)


natTag :: Tag
natTag = CTag{tCon = "nat" , tArity = 1}

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
leftUnitLaw (Unit (Var n) `Bind` LAltVar abs t) =
    applySubst rho $ leftUnitLaw t
  where
    rho = strengthenS impossible 1 `composeS` singletonS 0 (Var $ n + 1)

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

-- TODO Fix returning eval [Boquist 1999, p. 95]
specializeUpdate :: Term -> Term

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
specializeUpdate (caseUpdateView -> Just (mkUpdate, m, Case t1 t2 alts)) =
    specializeUpdate $ m $ Case t1 t2 alts'
  where
    alts' =
      for alts $ \case
        CAltConstantNode tag abss t3 ->
          let update = raise tag.tArity $ mkUpdate tag in
          CAltConstantNode tag abss $ update `BindEmpty` t3
        _ -> __IMPOSSIBLE__

-- update n₁ n₂ ; λ () →
-- unit n₂ ; λ CNat x →
-- 〈m 〉
-- >>>
-- updateᶜᴺᵃᵗ n₁ n₂ ; λ () →
-- unit n₂ ; λ CNat x →
-- 〈m 〉
specializeUpdate (
  Update Nothing n1 n2 `BindEmpty`
  Unit (Var n2') `Bind` LAltConstantNode tag abss t) =
  -- >>>
  Update (Just tag) n1 n2 `BindEmpty`
  Unit (Var n2') `Bind` LAltConstantNode tag abss (specializeUpdate t)

specializeUpdate (Bind t alt) =
  specializeUpdate t `Bind` specializeUpdateLalt alt
specializeUpdate (Case n t alts) =
  Case n (specializeUpdate t) (map specializeUpdateCalt alts)
specializeUpdate (Update mtag n1 n2)
  | Nothing <- mtag = __IMPOSSIBLE__
  | otherwise = Update mtag n1 n2
specializeUpdate t = t

specializeUpdateCalt :: CAlt -> CAlt
specializeUpdateCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ specializeUpdate t
specializeUpdateCalt (CAltLit lit t)       = CAltLit lit $ specializeUpdate t


specializeUpdateLalt :: LAlt -> LAlt
specializeUpdateLalt (LAltVar abs t)       = LAltVar abs $ specializeUpdate t
specializeUpdateLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ specializeUpdate t
specializeUpdateLalt (LAltEmpty t)         = LAltEmpty $ specializeUpdate t

-- Preconditions: Normalised
--
-- update n₁ n₂ ; λ () →
-- 〈m 〉where n₁ ∉ m
-- case n₂ of alts
--
-- Returns: (update n₁' n₂', 〈m 〉, case n₂ of alts)
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
      | otherwise = CAltTag tag t
    mkAlt _ alt = alt

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

    -- TODO
    go xs1 (t1 `Bind` (splitLaltWithAbss -> (mkAlt, t2, xs2))) = error "TODO need swap0n"
    go _ _ = __IMPOSSIBLE__

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





-- f #3 0 ; λ xs → <m>
-- >>>
-- unit #3 ; λ x →
-- f 0 1 ; λ xs → <m>
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


