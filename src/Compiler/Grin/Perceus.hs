{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Compiler.Grin.Perceus (ReferenceCountedTerm, mkDrop, perceus, specializeDrop, pushDownDup, fuseDupDrop) where

import           Control.Applicative          (Applicative (liftA2), (<|>))
import           Control.Monad                (filterM, forM, mapAndUnzipM,
                                               (<=<))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (MonadIO, MonadReader (ask),
                                               Reader, ReaderT (runReaderT),
                                               asks, local, runReader)
import           Data.Bool                    (bool)
import           Data.Foldable                (fold, foldlM, foldrM, foldl')
import           Data.Function                (on)
import           Data.List                    (partition, singleton, (\\))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Tuple.Extra             (both, dupe, first)
import           Prelude                      hiding ((!!))

import           Agda.Compiler.Backend        (Definition (theDef), MonadFresh,
                                               TCM, TPrim (PAdd, PSub, PSub64))
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal          (Literal (LitNat))
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Function          (applyUnless, applyWhen, iterate')
import           Agda.Utils.Functor
import           Agda.Utils.Impossible        (__IMPOSSIBLE__)
import           Agda.Utils.List
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.Maybe
import           Agda.Utils.Monad

import           Agda.Utils.Applicative       (forA)
import           Agda.Utils.Functor           (for)
import           Compiler.Grin.Grin
import           Control.Monad.Fix            (MonadFix)
import           Debug.Trace                  (traceM)
import           GHC.IO                       (unsafePerformIO)
import           Utils.List                   (zipWith3M)
import qualified Utils.List1                  as List1
import           Utils.Monad
import qualified Utils.Set                    as Set
import           Utils.Utils                  (logIO)

-- Properties:
-- • Δ ∩ Γ = ∅
-- • Γ ⊆ fv(t), thus a variable can only be added to Γ if it is a free variable of t
-- • fv(t) ⊆ Δ,Γ
-- • Δ and Γ are sets
data Context = Context
 { delta :: Set Abs
 , gamma :: Set Abs }

data Cxt = PerceusCxt
  { pc_vars           :: [Abs]
  , pc_pointers       :: Set Abs -- ^ Variables which are pointers
  , pc_returning      :: Bool
  , pc_tag_dependency :: Map Abs [Abs]
  }

initPerceusCxt :: GrinDefinition -> Cxt
initPerceusCxt def = PerceusCxt
  { pc_vars           = mempty
  , pc_pointers       = Set.fromList def.gr_args <> gatherPointers def.gr_term
  , pc_returning      = True
  , pc_tag_dependency = mempty
  }

varsLocal :: MonadReader Cxt m => [Abs] -> m a -> m a
varsLocal xs = local \ cxt -> cxt{pc_vars = cxt.pc_vars ++ xs}

varLocal :: MonadReader Cxt m => Abs -> m a -> m a
varLocal x = varsLocal [x]

deBruijnLookup :: MonadReader Cxt m => Int -> m (Maybe Abs)
deBruijnLookup n = asks $ (!!! n) . reverse . pc_vars

deBruijnOf :: Abs -> [Abs] -> Maybe Int
deBruijnOf x = genericElemIndex x . reverse

isPointer :: MonadReader Cxt m => Abs -> m Bool
isPointer x = asks $ Set.member x . pc_pointers

tagDependencyLocal :: MonadReader Cxt m => Abs -> [Abs] -> m a -> m a
tagDependencyLocal x xs = local $ \cxt -> cxt{pc_tag_dependency = Map.insert x xs cxt.pc_tag_dependency}

perceus :: (MonadFresh Int m) => GrinDefinition -> m GrinDefinition
perceus def = do
  t <- runReaderT (perceusDef def.gr_args def.gr_term) (initPerceusCxt def)
  pure $ setGrTerm t def

-- TODO replace with newtype or trees-that-grow
type ReferenceCountedTerm = Term

-- ∅ | Γ ⊢ t ⟿  t′   Γ = fv(t)   Γ′ = {x}∗ - Γ
-- -------------------------------------------
-- ø ⊢ f {x}∗ = t ⟿  f {x}∗ = drop Γ′; t′
perceusDef :: (MonadReader Cxt m, MonadFresh Int m) => [Abs] -> Term -> m ReferenceCountedTerm
perceusDef xs t = do
  -- Γ = fv(t)
  gamma <- varsLocal xs $ fvTerm t
  -- Γ′ = {x}∗ - Γ
  let gammad = Set.fromList xs Set.\\ gamma
  varsLocal xs $ do
    -- ∅ | Γ ⊢ t ⟿  t′
    t' <- perceusTerm (Context mempty gamma) t
    -- drop Γ′; t′
    dropSetL gammad t'



-- Rule: STORE
perceusStore 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Loc 
  -> Val
  -> m ReferenceCountedTerm
perceusStore c loc v = foldr BindEmpty (Store loc v) <$> perceusVal c v

-- Rule: UNIT
perceusUnit 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Val
  -> m ReferenceCountedTerm
perceusUnit c v = foldr BindEmpty (Unit v) <$> perceusVal c v

-- Rule: APP
perceusApp 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Val 
  -> [Val]
  -> m ReferenceCountedTerm
perceusApp c v vs = do
  -- FIXME GRIN.hs should use List1
  let vs' = List1.fromListSafe __IMPOSSIBLE__ vs
  cs <- splitContext c vs'
  gammad <- fold <$> List1.zipWithM perceusVal cs vs'
  pure (foldr BindEmpty (App v vs) gammad)

-- Rule: CASE
perceusCase 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Int
  -> Term 
  -> [CAlt]
  -> m ReferenceCountedTerm
perceusCase c n t alts = do
  -- Lookup the variables that depend on the scrutinee
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  xs <- asks $ fromMaybe mempty . Map.lookup x . pc_tag_dependency

  t' <- step (Set.fromList xs) t

  alts' <- forM alts \case
    CAltTag tag t -> CAltTag tag <$> step (Set.fromList $ drop tag.tArity xs) t
    CAltLit lit t -> CAltLit lit <$> step mempty t
    _             -> __IMPOSSIBLE__
  pure (Case (Var n) t' alts')
  where
  step _ Unreachable = pure Unreachable
  step bottoms t = do
      gammai <- Set.intersection c.gamma <$> fvTerm t
      t' <- perceusTerm (Context c.delta gammai) t
      let gammadi = c.gamma `Set.difference` (gammai `Set.union` bottoms)
      dropSetL gammadi t'

-- Rule: UPDATE
perceusUpdate
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Tag
  -> Tag
  -> Int
  -> Val 
  -> m ReferenceCountedTerm
perceusUpdate c tag' tag n v = do
  t <- foldr BindEmpty (Update tag' tag n v) <$> perceusVal c v
  foldrM step t [2 .. tag.tArity + 1]
  where
  -- TODO replace with a proper check using a lookup table or types
  isPointer _ = tag /= NatTag

  step i t
    | isPointer n = FetchOffset tag n i `bindVar` Drop 0 `BindEmpty` raise 1 t
    | otherwise   = pure t


-- Rule: BIND-FETCH-DUP
perceusBindFetchDup 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Tag
  -> Int
  -> Int
  -> Abs
  -> Term 
  -> m ReferenceCountedTerm
perceusBindFetchDup c tag n offset x t = do
  fv_t <- varLocal x (fvTerm t)
  let gamma' = Set.insert x (c.gamma `Set.intersection` fv_t)
      gammad = c.gamma `Set.difference` fv_t
  t' <- varLocal x $ 
    perceusTerm (Context c.delta gamma') t >>= (`dropSetR` gammad)
  pure (FetchOffset tag n offset `Bind` LAltVar x 
       (Dup 0 `BindEmpty` t'))
  
-- Rule: BIND-FETCH
perceusBindFetch
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Tag
  -> Int
  -> Int
  -> Abs
  -> Term 
  -> m ReferenceCountedTerm
perceusBindFetch c tag n offset x t = do
  fv_t <- varLocal x (fvTerm t)
  let gamma' = c.gamma `Set.intersection` fv_t
      gammad = c.gamma `Set.difference` fv_t
      delta' = Set.insert x c.delta
  t' <- varLocal x $ 
    perceusTerm (Context delta' gamma') t >>= (`dropSetR` gammad)
  pure (FetchOffset tag n offset `Bind` LAltVar x t')

-- Rule: BIND
perceusBind
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Term
  -> LAlt 
  -> m ReferenceCountedTerm
perceusBind c t1 alt = do
  let (mkAlt, t2, xs) = splitLaltWithVars alt
  bv_p <- Set.fromList <$> filterM isPointer xs

  ov_t1 <- ov t1
  fv_t2 <- varsLocal xs (fvTerm t2)

  let gamma2 = (c.gamma `Set.union` bv_p) `Set.intersection` fv_t2
      gamma1 = (c.gamma `Set.intersection` ov_t1) `Set.difference` gamma2
      gammad = (c.gamma `Set.union` bv_p) `Set.difference` (gamma1 `Set.union` gamma2)

  -- logIO ""
  -- logIO $ "t1:\n" ++ render (nest 4 (pretty t1))
  -- logIO $ "alt:\n" ++ render (nest 4 (pretty (mkAlt t2)))
  -- logIO $ "gamma1: " ++ prettyShow gamma1
  -- logIO $ "gammad: " ++ prettyShow gammad
  -- logIO $ "gamma2: " ++ prettyShow gamma2

  t1' <- perceusTerm
           do Context (c.delta `Set.union` gammad `Set.union` gamma2) gamma1
           do t1

  -- Add the the tag dependency x to the arguments xs, so when we pattern match on x
  -- we can determine how many of xs are defined.
  let withTagDependency = case alt of
        LAltVariableNode x xs _ -> tagDependencyLocal x xs
        _                       -> id

  t2' <- withTagDependency $ varsLocal xs $ dropSetL gammad =<< perceusTerm (Context c.delta gamma2) t2
  pure (t1' `Bind` mkAlt t2')


-- Δ | Γ ⊢ t ⟿  t′
perceusTerm 
  :: (MonadReader Cxt m, MonadFresh Int m) 
  => Context 
  -> Term 
  -> m ReferenceCountedTerm
-- Rule: FETCH-OPAQUE
perceusTerm _ (FetchOpaqueOffset n offset) = pure (FetchOpaqueOffset n offset) 
-- Rule: UNREACHABLE 
perceusTerm _ (Error e) = pure (Error e) 
perceusTerm c (Store loc v) = perceusStore c loc v
perceusTerm c (Unit v) = perceusUnit c v
perceusTerm c (App v vs) = perceusApp c v vs
perceusTerm c (Case (Var n) t alts) = perceusCase c n t alts
perceusTerm c (Update tag' tag n v) = perceusUpdate c tag' tag n v
perceusTerm c (FetchOffset tag n offset `Bind` LAltVar x t) = 
  ifM do elem x <$> varLocal x (fvTerm t) 
      do perceusBindFetchDup c tag n offset x t
      do perceusBindFetch c tag n offset x t
perceusTerm c (t `Bind` alt) = perceusBind c t alt
-- Only allowed after the Perceus algorithm
perceusTerm _ Dup{} = __IMPOSSIBLE__
perceusTerm _ Decref{} = __IMPOSSIBLE__
-- Fetch entire nodes are not allowed in lower-level GRIN.
-- P-tags are not yet implemented. All other matches should
-- be handled by the other rules.
perceusTerm _ Fetch'{} = __IMPOSSIBLE__
-- Case must scrutinize variables otherwise they should be
-- simplified by constant propagation.
perceusTerm _ Case{} = __IMPOSSIBLE__


type Dups = [ReferenceCountedTerm]

-- TODO formalize
perceusVal :: MonadReader Cxt m => Context -> Val -> m Dups
perceusVal c (Var n) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  p <- isPointer x
  let not_ptr = boolToMaybe (not p) []
      svar = boolToMaybe (p && c.gamma == Set.singleton x) []
      svar_dup = boolToMaybe (p && Set.member x c.delta) [Dup n]

  let s = render $ vcat
        [ text "VAR" <+> pretty x
        , text "isPointer" <+> pretty p
        , text "c.delta" <+> pretty c.delta
        , text "c.gamma" <+> pretty c.gamma
        ]

  pure $ fromMaybe (error s) (not_ptr <|> svar <|> svar_dup)

perceusVal c (ConstantNode _ (v : vs)) = do
  contexts <- splitContext c (v :| vs)
  -- logIO $ render $ vcat
  --   [ text "\nCONSTANTNODE" <+> pretty val
  --   , text "c.delta" <+> pretty c.delta
  --   , text "c.gamma" <+> pretty c.gamma
  --   , text "contexts:" <+> vcat (List1.map (\c -> pretty c.delta <+> pretty c.gamma) contexts)
  --   ]
  fold <$> List1.zipWithM perceusVal contexts (v :| vs)

-- I think variable nodes are never retured or stored so they cannot consume values
perceusVal _ VariableNode{} = pure []
perceusVal _ _ = pure []






-- | Input context Δ | Γ and non-empty list of values {v}+ are split into
--   multiple contexts.
--
-- (Δ,Γ₂,‥,Γₙ | Γ₁), (Δ,Γ₃,‥,Γₙ | Γ₂),‥, (Δ | Γₙ)
--
-- where |{v}+| = n and Γᵢ = (Γ - Γᵢ₊₁,‥,Γₙ) ∩ fv(vᵢ)
--
splitContext :: MonadReader Cxt m => Context -> List1 Val -> m (List1 Context)
splitContext c vs = List1.scanr' step (Context c.delta) <$> gammas
  where
  -- Δᵢ = Δᵢ₊₁,Γᵢ₊₁
  step gammai c = Context (c.delta <> c.gamma) gammai

  -- Γₙ = Γ ∩ fv(vₙ)
  gamman = Set.intersection c.gamma <$> fvVal (List1.last vs)

  -- Γᵢ,‥, Γₙ where Γᵢ = (Γ - Γᵢ₊₁ - ‥ - Γₙ) ∩ fv(vᵢ)
  gammas = (foldr step . List1.singleton <$> gamman) <*> mapM fvVal (List1.init vs)
    where
    -- Γᵢ = (Γ - Γᵢ₊₁,‥,Γₙ) ∩ fv(vᵢ)
    step :: Set Abs -> List1 (Set Abs) -> List1 (Set Abs)
    step fv acc = Set.intersection (c.gamma Set.\\ fold acc) fv <| acc


-- | drop Γ; t = drop x₁; ..; drop xₙ ; t   where |Γ| = n
dropSetL :: MonadReader Cxt m => Set Abs -> Term -> m ReferenceCountedTerm
dropSetL gammad t = asks \ cxt ->
  foldr (step cxt.pc_vars) t gammad
  where
  step xs x t =
    caseMaybe do deBruijnOf x xs
              do error $ unwords ["dropSetL: Cannot find", prettyShow x, "in", prettyShow xs]
              do \ n -> Drop n `BindEmpty` t

-- | drop Γ; t = t ; drop x₁ ; ... ; drop xₙ    where |Γ| = n
dropSetR :: MonadReader Cxt m => Term -> Set Abs -> m ReferenceCountedTerm
dropSetR t gammad = asks \ cxt ->
  foldr (step cxt.pc_vars) t gammad
  where
  step xs x t =
    caseMaybe do deBruijnOf x xs
              do error $ unwords ["dropSetR: Cannot find", prettyShow x, "in", prettyShow xs]
              do \ n -> Drop n `BindEmpty` t


-- | Returns the free variables that the term want to own.
-- ov :: Term -> Reader Cxt (Set Abs)
ov :: MonadReader Cxt m => Term -> m (Set Abs)
ov (App _ vs) = foldMapM fvVal vs
ov (Store _ (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Unit (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Update _ _ _ (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Case _ t alts) =
  liftA2 Set.union (ov t) $ foldMapM (ov . snd . splitCalt) alts
ov (t1 `Bind` (splitLaltWithVars -> (_, t2, xs))) = do
  xs1 <- ov t1
  xs2 <- varsLocal xs (ov t2) <&> (`Set.difference` Set.fromList xs)
  pure (xs1 <> xs2)
ov _  = pure mempty

-- | Returns the free variables of a terms that are pointers.
fvTerm :: MonadReader Cxt m => Term -> m (Set Abs)
fvTerm (t1 `Bind` (splitLaltWithVars -> (_, t2, xs))) = do
  xs1 <- fvTerm t1
  xs2 <- (Set.\\ Set.fromList xs) <$> varsLocal xs (fvTerm t2)
  -- unnecessary?
  Set.filterM isPointer (xs1 <> xs2)
fvTerm (Case v t alts) = do
  xs1 <- fvVal v
  xs2 <- fvTerm t
  xs3 <- foldMapM (fvTerm . snd . splitCalt) alts
  -- unnecessary?
  Set.filterM isPointer (xs1 <> xs2 <> xs3)
fvTerm (App _ vs) = foldMapM fvVal vs
fvTerm (Unit v) = fvVal v
fvTerm (Store _ v) = fvVal v
fvTerm (Fetch' _ n (Just _)) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  unlessM (isPointer x) __IMPOSSIBLE__
  pure (Set.singleton x)
fvTerm t@(Update _ _ n v) = do
  xs <- asks pc_vars
  x <- fromMaybe (error $ unwords ["can't find", show n, "in", prettyShow xs, "\nTerm:", prettyShow t]) <$> deBruijnLookup n
  unlessM (isPointer x) __IMPOSSIBLE__
  Set.insert x <$> fvVal v
fvTerm Error{} = pure mempty
fvTerm t = error $ "missing " ++ show t
-- fvTerm _ = __IMPOSSIBLE__

-- Only returns free variables that are pointers
fvVal :: MonadReader Cxt m => Val -> m (Set Abs)
fvVal (Var n) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  bool mempty (Set.singleton x) <$> isPointer x
fvVal (ConstantNode _ vs) = foldMapM fvVal vs
fvVal (VariableNode _ vs) = foldMapM fvVal vs
fvVal _ = pure mempty


-- | This function gathers the set of pointers which are created in a given term.
--
-- We need to distinguish between register stored values and references.
-- The basic rule is that `store` create references which are then kept
-- in offset 2 and 3 of a node. Additionally, the function return value is
-- stored in registers, but it may contain references.
--
-- Register values:
-- • Offset 0 and 1 in all nodes
-- • Offset 2 in the Cnat node
-- • The tag of a variable node
-- • Both operands and the result of a primitive operation
-- • Currently, unit never binds pointers (due to left unit law transformation),
--   however this maybe change with CSE.
gatherPointers :: Term -> Set Abs
gatherPointers (Store _ _ `Bind` LAltVar x t) = Set.insert x (gatherPointers t)
gatherPointers (FetchOffset tag _ _ `Bind` LAltVar _ t) | tag == natTag = gatherPointers t
gatherPointers (Fetch' _ _ (Just offset) `Bind` LAltVar x t)
  | offset >= 2 = Set.insert x (gatherPointers t)
gatherPointers (App Def{} _ `Bind` alt) = gatherPointersLalt alt

-- Unsure about this one
gatherPointers (Case _ t alts `Bind` alt) =
  gatherPointers t <> foldMap gatherPointersCalt alts <> gatherPointersLalt alt

-- Recurse
gatherPointers (Case _ t alts) = gatherPointers t <> foldMap gatherPointersCalt alts
gatherPointers (t1 `Bind` (snd . splitLalt -> t2)) = on (<>) gatherPointers t1 t2

gatherPointers _ = mempty

gatherPointersLalt :: LAlt -> Set Abs
gatherPointersLalt (LAltVar _ t) = gatherPointers t
gatherPointersLalt (LAltConstantNode tag xs t)
  | tag == natTag = gatherPointers t
  | otherwise     = Set.fromList xs <> gatherPointers t
gatherPointersLalt (LAltVariableNode _ xs t) = Set.fromList xs <> gatherPointers t
gatherPointersLalt (LAltEmpty t) = gatherPointers t

-- There should only exist tags without binders
gatherPointersCalt :: CAlt -> Set Abs
gatherPointersCalt (snd . splitCalt -> t) = gatherPointers t

-- FIXME specializeDrop should use tagInfo instead
specializeDrop :: forall mf. MonadFresh Int mf =>  GrinDefinition -> mf GrinDefinition
specializeDrop def = lensGrTerm (go $ replicate def.gr_arity Nothing) def
  where
  go :: [Maybe (Tag, [Int])] -> Term -> mf Term
  go xs (Update tag' tag n (Cnat v) `BindEmpty` t) =
    (Update tag' tag n (Cnat v) `BindEmpty`) <$> go xs' t
    where
    xs' = updateAt n (const $ Just (NatTag, [])) xs

  go xs (Drop n `BindEmpty` t)
    | Just (tag, map (n +) -> ns) <- xs !! n = do

      let unique = raise 1 $ foldr (BindEmpty . Drop) (free n) (reverse ns)

      drop <-
        FetchOffset tag n 0 `bindVar`
        Case (Var 0) (Decref $ n + 1) [CAltLit (LitNat 1) unique]

      drop `bindEmptyR` go xs t

    | otherwise = pure (Drop n `BindEmpty` t)



  go xs (t1 `Bind` (splitLaltWithVars -> (mkAlt, t2, length -> n))) = do
    t1' <- go (replicate n Nothing ++ xs) t1
    t2' <- go (replicate n Nothing ++ xs) t2
    pure (t1' `Bind` mkAlt t2')

  go xs (Case v t alts) = do
    t' <- go xs t
    alts' <- mapM step alts
    pure (Case v t' alts')
    where
    step (splitCalt -> (mkAlt, t)) = mkAlt <$> go xs t

  go _ t = pure t

pushDownDup :: ReferenceCountedTerm -> ReferenceCountedTerm
pushDownDup (Dup n1 `BindEmpty` (pushDownDup -> t1)) =
  fromMaybe (Dup n1 `BindEmpty` t1) (go t1)
  where
  go (Update tag' tag n2 v `BindEmpty`
           (FetchOffset tag'' n2' 0 `Bind` LAltVar x
           (Case (Var 0) t2 alts `BindEmpty` t3)))
    | n2 == n2'
    , tag' == tag'' =
      Just (Update tag' tag n2 v `BindEmpty`
           (FetchOffset tag'' n2' 0 `Bind` LAltVar x
           (Case (Var 0) t2' alts' `BindEmpty` t3)))
    where
    t2' = Dup (n1 + 1) `BindEmpty` t2
    alts' = map (\(splitCalt -> (mkAlt, t)) -> mkAlt (Dup (n1 + 1) `BindEmpty` t))  alts
  go _ = Nothing

pushDownDup (t1 `Bind` (splitLalt -> (mkAlt, t2))) = pushDownDup t1 `Bind` mkAlt (pushDownDup t2)

pushDownDup (Case v t alts) = Case v (pushDownDup t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (pushDownDup t)

pushDownDup t = t

fuseDupDrop :: ReferenceCountedTerm -> ReferenceCountedTerm
fuseDupDrop (Dup n `BindEmpty` (fuseDupDrop -> t)) = fromMaybe (Dup n `BindEmpty` t) (go t)
  where
  -- TODO more patterns are valid!
  go (Drop n' `BindEmpty` t)
    | n' == n = Just t
    | otherwise = BindEmpty (Drop n') <$> go t
  go (Dup n' `BindEmpty` t) = BindEmpty (Dup n') <$> go t
  go _ = Nothing

fuseDupDrop (t1 `Bind` (splitLalt -> (mkAlt, t2))) = fuseDupDrop t1 `Bind` mkAlt (fuseDupDrop t2)

fuseDupDrop (Case v t alts) = Case v (fuseDupDrop t) (map step alts)
  where
  step (splitCalt -> (mkAlt, t)) = mkAlt (fuseDupDrop t)

fuseDupDrop t = t







{-
drop x₀ =
  fetch 0 [1] ; λ x₁ →
  case 0 of
    0 →
      fetch 1 [0] ; x₂ →
      case 0 of
        _∷_ →
          fetch 2 [2] ; λ x₃ →
          drop 0 ; λ () →
          fetch 3 [3] ; λ x₄ →
          drop 0 ; λ () →
          free 4
        [] →
          free 2
        downFrom →
          fetch 2 [2] ; λ x₅ →
          drop 0 ; λ () →
          free 4
        ...
    _ →
      decref 0
-}



mkDrop :: forall mf. MonadFresh Int mf => [GrinDefinition] -> mf GrinDefinition
mkDrop defs = do
  unique <-
    FetchOpaqueOffset 1 1 `bindVarR`
    Case (Var 0) Unreachable <$> mapM mkAlt tags
  term <-
    FetchOpaqueOffset 0 0 `bindVar`
    Case (Var 0) (Decref 1) [CAltLit (LitNat 1) unique]

  arg <- freshAbs
  pure GrinDefinition
    { gr_name = "drop"
    , gr_isMain = False
    , gr_primitive = Nothing
    , gr_arity = 1
    , gr_type = Nothing
    , gr_term = term
    , gr_args = [arg]
    , gr_return = Nothing
    }
  where
  tags = Set.toList $ foldMap (gatherTags . gr_term) defs

  -- TODO big layout
  mkAlt :: Tag -> mf CAlt
  mkAlt tag | tag == natTag = pure $ CAltTag tag (free 2)
  mkAlt tag = CAltTag tag <$> foldrM dropChild (free 2) (take arity [2 .. ])
    where
    arity = tagArity tag
    dropChild offset t =
      FetchOffset tag 2 offset `bindVar`
      Drop 0                   `BindEmpty`
      raise 1 t

{- Maybe remove. Would be nice to ensure invariants better than the current approach.


-- contextMember x c = on (||) (Set.member x) (c ^. deltaLens) (c ^. gammaLens)
--
contextMember :: Abs -> Context -> Bool
contextMember x c = on (||) (Set.member x) c.delta c.gamma

deltaLens :: Lens' Context (Set Abs)
deltaLens f c = f c.delta <&> \delta -> c{delta = delta}

gammaLens :: Lens' Context (Set Abs)
gammaLens f c = f c.gamma <&> \gamma -> c{gamma = gamma}


deltaInsert :: Abs -> Context -> Context
deltaInsert x c | contextMember x c = c
                | otherwise = over deltaLens (Set.insert x) c

-- Precondition: x ∈ fv(t)
gammaInsert :: Abs -> Context -> Context
gammaInsert x c | contextMember x c = c
                | otherwise = over gammaLens (Set.insert x) c

-}
