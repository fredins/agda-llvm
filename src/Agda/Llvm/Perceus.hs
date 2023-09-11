{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Agda.Llvm.Perceus (perceus, mkDrop, mkDup) where

import           Control.Applicative          (Applicative (liftA2), (<|>))
import           Control.Monad                (filterM, (<=<))
import           Control.Monad.Reader         (MonadIO (liftIO), MonadReader,
                                               Reader, ReaderT (runReaderT),
                                               asks, local, runReader)
import           Data.Bool                    (bool)
import           Data.Foldable                (fold, foldrM)
import           Data.Function                (on)
import           Data.List                    (partition, singleton, (\\))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set


import           Data.Tuple.Extra             (both, dupe, first)


import           Agda.Compiler.Backend        (MonadFresh, TCM,
                                               TPrim (PAdd, PSub, PSub64))
import           Agda.Llvm.Grin
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal          (Literal (LitNat))
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.Maybe
import           Agda.Utils.Monad

-- Properties:
-- • Δ ∩ Γ = ∅
-- • Γ ⊆ fv(t), thus a variable can only be added to Γ if it is a free variable of t
-- • fv(t) ⊆ Δ,Γ
-- • Δ and Γ are sets
data Context = Context
 { delta :: Set Abs
 , gamma :: Set Abs }

data PerceusCxt = PerceusCxt
  { pc_vars             :: [Abs]
  , pc_pointers         :: Set Abs
  , pc_returning        :: Bool -- ^ Variables which are pointers
  , pc_tagDependentVars :: Map Abs [Abs]
  }

initPerceusCxt :: GrinDefinition -> PerceusCxt
initPerceusCxt def = PerceusCxt
  { pc_vars = def.gr_args
  , pc_pointers = Set.fromList def.gr_args <> gatherPointers def.gr_term
  , pc_returning = True
  , pc_tagDependentVars = mempty
  }

varsLocal :: MonadReader PerceusCxt m => [Abs] -> m a -> m a
varsLocal xs = local $ \cxt -> cxt{pc_vars = cxt.pc_vars ++ xs}

deBruijnLookup :: Int -> Perceus (Maybe Abs)
deBruijnLookup n = asks $ (!!! n) . reverse . pc_vars

deBruijnOf :: Abs -> [Abs] -> Maybe Int
deBruijnOf x = genericElemIndex x . reverse

isPointer :: Abs -> Perceus Bool
isPointer x = asks $ Set.member x . pc_pointers

tagDependentVarsLookup :: Abs -> Perceus [Abs]
tagDependentVarsLookup x = asks $ fromMaybe [] . Map.lookup x . pc_tagDependentVars

-- TODO remove TCM! only used for debugging
perceus :: GrinDefinition -> TCM GrinDefinition
perceus def = do
  t <- term
  pure def{gr_term = t}
  where
  -- ∅ | Γ ⊢ t ⟿  t′   Γ = fv(t)   Γ′ = {x}∗ - Γ
  -- -------------------------------------------
  -- ø ⊢ f {x}∗ = t ⟿  f {x}∗ = drop Γ′; t′

  term = flip runReaderT (initPerceusCxt def) $ do
    xs <- fvTerm def.gr_term
    -- Γ = fv(t)   Γ′ = {x}∗ - Γ
    let (gamma, gamma') = both Set.fromList $ partition (`Set.member` xs) def.gr_args
    varsLocal def.gr_args $ do
      -- ∅ | Γ ⊢ t ⟿  t′
      t' <- perceusTerm (Context mempty gamma) def.gr_term
      -- drop Γ′; t′
      dropSet gamma' t'


type Perceus a = ReaderT PerceusCxt TCM a

-- Δ | Γ ⊢ t ⟿  t′
perceusTerm :: Context -> Term -> Perceus Term


-- Dup arguments with appropriate contexts.
--
-- γᵢ ⊆ Γᵢ
-- Δ,Γᵢ₊₁,‥,Γₙ | Γᵢ ⊢ vᵢ ⟿  dup γᵢ
-- --------------------------------------
-- Δ Γ ⊢ f {v}∗ ⟿  dup {γ}∗ ; f {v}∗
perceusTerm c term@(App (Def f) vs) = do
  let vs' = List1.fromListSafe __IMPOSSIBLE__ vs
  cs <- splitContext c vs'
  -- logIO $ render $ vcat
  --   [ text "\nAPP" <+> pretty term
  --   , text "c.delta" <+> pretty c.delta
  --   , text "c.gamma" <+> pretty c.gamma
  --   , text "contexts:" <+> vcat (List1.map (\c -> pretty c.delta <+> pretty c.gamma) cs)
  --   ]

  dups <- fold <$> List1.zipWithM perceusVal cs vs'
  pure (foldr BindEmpty (App (Def f) vs) dups)

-- Currently, only returning unit with a constant node is possible so
-- this rule only applies to that case.
--
-- --------------------------------
-- Δ | Γ ⊢ unit v ⟿  dup v ; dup v; unit v

-- γ ⊆ Γ
-- Δ | Γ ⊢ v ⟿  dup γ
-- --------------------------------------
-- Δ Γ ⊢ unit v ⟿  dup fv(v) ; dup γ ; f {v}∗
perceusTerm c (Unit v) = do
  fv <- fvVal v
  --- vars <- asks pc_vars
  -- logIO $ render $ vcat
  --   [ text "UNIT" <+> pretty (Unit v)
  --   , text "c.delta" <+> pretty c.delta
  --   , text "c.gamma" <+> pretty c.gamma
  --   , text "fv" <+> pretty fv
  --   , text "vars" <+> pretty vars
  --   ]
  dupSet fv . foldr BindEmpty (Unit v) =<< perceusVal c v


-- ----------------------------------
-- Δ | Γ ⊢ store v ⟿  dup v ; store v
perceusTerm c (Store loc v) =
  foldr BindEmpty (Store loc v) <$> perceusVal c v


-- Drops references which are not needed in t₂, both
-- from the newly bound variables {x}∗ and the owned
-- environment Γ. It make sure to not drop any
-- referernces which t₁ is responsible for dropping.
--
-- Γ₂ = Γ ∩ fv(t₂)
-- Γ₂′ = Γ - Γ₂,ov(t₁)
-- Γₓ = fv(t₂) ∩ {x}∗    Δ,Γ₂,Γ₂′ | Γ - Γ₂,Γ₂′ ⊢ t₁ ⟿  t₁′
-- Γₓ′ = fv(t₂) - {x}∗               Δ | Γ₂,Γₓ ⊢ t₂ ⟿  t₂′
-- -------------------------------------------------------------
-- Δ | Γ ⊢ t₁ ; λ {x}∗ → t₂ ⟿  t₁′ ; λ {x}∗ → drop Γ₂′,Γₓ′ ; t₂′
perceusTerm c term@(t1 `Bind` alt) = do
  let (mkAlt, t2, xs) = splitLaltWithVars alt

  ov1 <- ov t1
  fv2 <- varsLocal xs (fvTerm t2)
  xs' <- Set.fromList <$> filterM isPointer xs

  let tagDependentVarsLocal = case alt of
        LAltVariableNode x xs _ ->
          let xs'' = filter (`Set.member` xs') xs in
          local $ \cxt -> cxt{pc_tagDependentVars =  Map.insert x xs'' cxt.pc_tagDependentVars}
        _ -> id

  -- Γ₂ = Γ ∩ fv(t₂)
  -- Γ₂′ = Γ - Γ₂,ov(t₁)
  let gamma2  = Set.intersection c.gamma fv2
      gamma2' = c.gamma Set.\\ (gamma2 <> ov1)

  -- Γₓ = fv(t₂) ∪ {x}∗
  -- Γₓ′ = fv(t₂) - {x}∗
  let (gammax, gammax') = Set.partition (`Set.member` fv2) xs'

  -- logIO $ render $ vcat
  --   [ text "\nBIND:"
  --   , nest 4 (pretty term)
  --   , text "c.delta" <+> pretty c.delta
  --   , text "c.gamma" <+> pretty c.gamma
  --   , text "ov1"  <+> pretty ov1
  --   , text "fv2"  <+> pretty fv2
  --   , text "gamma2" <+> pretty gamma2
  --   , text "gamma2'" <+> pretty gamma2'
  --   , text "gammax" <+> pretty gammax
  --   , text "gammax'" <+> pretty gammax' ]

  -- Δ,Γ₂,Γ₂′ | Γ - Γ₂,Γ₂′ ⊢ t₁ ⟿  t₁′
  let context = Context (c.delta <> gamma2 <> gamma2') (c.gamma Set.\\ (gamma2 <> gamma2'))

  -- logIO $ render $ vcat
  --   [ text "\nLEFT TERM:"
  --   , nest 4 (pretty t1)
  --   , text "delta" <+> pretty context.delta
  --   , text "gamma" <+> pretty context.gamma
  --   , text "\nRIGHT TERM:"
  --   , nest 4 (pretty t2)
  --   , text "delta" <+> pretty c.delta
  --   , text "gamma" <+> pretty (gamma2 <> gammax)
  --   ]

  t1' <- perceusTerm context t1

  -- Δ | Γ₂ ⊢ t₂ ⟿  t₂′
  t2' <- varsLocal xs $ tagDependentVarsLocal $ perceusTerm (Context c.delta $ gamma2 <> gammax) t2
  -- drop Γ₂′,Γₓ′ ; t₂′
  t2'_drop <- varsLocal xs $ dropSet (gamma2' <> gammax') t2'

  pure (t1' `Bind` mkAlt t2'_drop)



-- Δ | Γᵢ ⊢ tᵢ ⟿  tᵢ′   Γᵢ = Γ ∩ fv(tᵢ)    Γᵢ′ = Γ - Γᵢ
-- ----------------------------------------------------------------------
-- Δ | Γ ⊢ case x of t {tagᵢ → tᵢ}* ⟿  case x of t {tagᵢ → drop Γᵢ′ ; tᵢ′}*

perceusTerm c (Case (Var n) t alts) = do
  x <- fromMaybe __IMPOSSIBLE__  <$> deBruijnLookup n
  xs <- tagDependentVarsLookup x

  t' <- if t == unreachable then pure t else stepBody (Set.fromList xs) t
  alts' <- mapM (stepAlt xs) alts

  pure (Case (Var n) t' alts')
  where
  stepAlt xs (CAltTag tag t) = CAltTag tag <$> stepBody bottoms t
    where
    bottoms = Set.fromList (xs \\ take (tagArity tag) xs)
  stepAlt xs (CAltLit lit t) = CAltLit lit <$> stepBody (Set.fromList xs) t
  stepAlt _ _ = __IMPOSSIBLE__


  -- Δ | Γᵢ ⊢ tᵢ ⟿  drop Γᵢ′ ; tᵢ′
  stepBody bottoms t = do
      -- Γᵢ = (Γ - {⊥}∗) ∩ fv(tᵢ)
      fv <- fvTerm t
      let gammai = Set.intersection (c.gamma Set.\\ bottoms) fv

      -- Δᵢ = Γ - {⊥}∗
      let deltai = c.delta Set.\\ bottoms

      -- logIO $ render $ vcat
      --   [ text "CALT"
      --   , text "bottoms" <+> pretty bottoms
      --   , nest 4 (pretty t)
      --   , text "c.delta" <+> pretty c.delta
      --   , text "c.gamma" <+> pretty c.gamma
      --   ]

      -- Δ | Γᵢ ⊢ tᵢ ⟿  tᵢ′
      t' <- perceusTerm (Context deltai gammai) t

      -- Γᵢ′ = Γ - {⊥} - Γᵢ ∗
      let gammai' = c.gamma Set.\\ bottoms Set.\\ gammai

      -- drop Γᵢ′ ; tᵢ′
      dropSet gammai' t'


-- These only borrows values, and thereby don't create any dups.
-- x ∈ Δ is ensured by fv(t) in the other patterns.
perceusTerm _ (Fetch' mtag n (Just offset)) = pure $ Fetch' mtag n (Just offset)
perceusTerm _ (Update tag n v) = pure (Update tag n v)
perceusTerm _ UpdateOffset{} = __IMPOSSIBLE__
perceusTerm _ (Error e) = pure (Error e)
perceusTerm _ (App (Prim prim) vs) = pure (App (Prim prim) vs)

perceusTerm _ Case{} = __IMPOSSIBLE__
perceusTerm _ Fetch'{} = __IMPOSSIBLE__
perceusTerm _ App{} = __IMPOSSIBLE__

type Dups = [Term]

perceusVal :: Context -> Val -> Perceus Dups
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

perceusVal c val@(ConstantNode _ (v : vs)) = do
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
-- where |{v}+| = n and Γᵢ = (Γ - Γᵢ₊₁ - ‥ - Γₙ) ∩ fv(vᵢ)   -- TODO maybe ov(vᵢ)
--
splitContext :: Context -> List1 Val -> Perceus (List1 Context)
splitContext c vs = list1scanr step (Context c.delta) <$> gammas
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
dropSet :: Set Abs -> Term -> Perceus Term
dropSet set t = do
  xs <- asks pc_vars
  let drops = Set.map (Drop . fromMaybe __IMPOSSIBLE__ . flip deBruijnOf xs) set
  pure $ foldr BindEmpty t drops


-- | dup Γ; t = dup x₁; ..; dup xₙ ; t   where |Γ| = n
dupSet :: Set Abs -> Term -> Perceus Term
dupSet set t = do
  xs <- asks pc_vars
  let drops = Set.map (Dup . fromMaybe __IMPOSSIBLE__ . flip deBruijnOf xs) set
  pure $ foldr BindEmpty t drops


-- | Returns the free variables that the term want to own.
ov :: Term -> Perceus (Set Abs)
ov (App _ vs) = foldMapM fvVal vs
ov (Store _ (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Unit (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Update _ _ (ConstantNode _ vs)) = foldMapM fvVal vs
ov (Case _ t alts) = ov t <> foldMapM (ov . snd . splitCalt) alts
ov (t1 `Bind` (splitLaltWithVars -> (_, t2, xs))) = ov t1 <> varsLocal xs (ov t2)
ov _  = pure mempty

-- | Returns the free variables of a terms that are pointers.
fvTerm :: Term -> Perceus (Set Abs)
fvTerm (t1 `Bind` (splitLaltWithVars -> (_, t2, xs))) = do
  xs1 <- fvTerm t1
  xs2 <- (Set.\\ Set.fromList xs) <$> varsLocal xs (fvTerm t2)
  setfilterM isPointer (xs1 <> xs2)
fvTerm (Case v t alts) = do
  xs1 <- fvVal v
  xs2 <- fvTerm t
  xs3 <- foldMapM (fvTerm . snd . splitCalt) alts
  setfilterM isPointer (xs1 <> xs2 <> xs3)
fvTerm (App _ vs) = foldMapM fvVal vs
fvTerm (Unit v) = fvVal v
fvTerm (Store _ v) = fvVal v
fvTerm (Fetch' _ n (Just _)) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  unlessM (isPointer x) __IMPOSSIBLE__
  pure (Set.singleton x)
fvTerm (Update _ n v) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  unlessM (isPointer x) __IMPOSSIBLE__
  Set.insert x <$> fvVal v
fvTerm Error{} = pure mempty
fvTerm _ = __IMPOSSIBLE__

-- Only returns free variables that are pointers
fvVal :: Val -> Perceus (Set Abs)
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
  decref' <- decref
  unique' <- unique
  term <- FetchOpaqueOffset 0 0 `bindVar`
          Case (Var 0) decref' [CAltLit (LitNat 1) unique']

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

  decref :: mf Term
  decref =
    App (Prim PSub) [Var 0, mkLit 1] `bindVar`
    UpdateOffset 2 0 (Var 0)

  unique :: mf Term
  unique =
    FetchOpaqueOffset 1 1 `bindVarR`
    Case (Var 0) unreachable <$> mapM mkAlt tags

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

mkDup :: forall mf. MonadFresh Int mf => mf GrinDefinition
mkDup = do
  term <-
    FetchOpaqueOffset 0 0            `bindVarR`
    App (Prim PAdd) [Var 0, mkLit 1] `bindVar`
    UpdateOffset 2 0 (Var 0)
  arg <- freshAbs
  pure GrinDefinition
    { gr_name = "dup"
    , gr_isMain = False
    , gr_primitive = Nothing
    , gr_arity = 1
    , gr_type = Nothing
    , gr_term = term
    , gr_args = [arg]
    , gr_return = Nothing }





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
