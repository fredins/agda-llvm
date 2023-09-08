{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}

module Agda.Llvm.Perceus (perceus, mkDrop) where

import           Control.Applicative          ((<|>))
import           Control.Monad                (filterM)
import           Control.Monad.Reader         (MonadReader, Reader, asks, local,
                                               runReader)
import           Data.Bool                    (bool)
import           Data.Foldable                (fold, foldrM)
import           Data.Function                (on)
import           Data.List                    (partition, singleton)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Tuple.Extra             (first)


import           Agda.Compiler.Backend        (MonadFresh)
import           Agda.Llvm.Grin
import           Agda.Llvm.Utils
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

-- Only returns free variables that are pointers
fvTerm :: Term -> Perceus (Set Abs)
fvTerm (t1 `Bind` (splitLaltWithVars -> (_, t2, xs))) = do
  xs1 <- fvTerm t1
  xs2 <- (Set.\\ Set.fromList xs) <$> varsLocal xs (fvTerm t2)
  pointers <- asks pc_pointers
  pure $ Set.filter (`Set.member` pointers) (xs1 <> xs2)
fvTerm (Case v t alts) = do
  xs1 <- fvVal v
  xs2 <- fvTerm t
  xs3 <- foldMapM (fvTerm . snd . splitCalt) alts
  pointers <- asks pc_pointers
  pure $ Set.filter (`Set.member` pointers) (xs1 <> xs2 <> xs3)
fvTerm (App _ vs) = foldMapM fvVal vs
fvTerm (Unit v) = fvVal v
fvTerm (Store _ v) = fvVal v
fvTerm (Fetch n _) = do
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
fvVal (ConstantNode tag _ (_ : _)) | tag == natTag = pure mempty
fvVal (ConstantNode _ _ vs) = foldMapM fvVal vs
fvVal (VariableNode n _ vs) = foldMapM fvVal (Var n : vs)
fvVal _ = pure mempty

data PerceusCxt = PerceusCxt
  { pc_vars     :: [Abs]
  , pc_pointers :: Set Abs } -- ^ Variables which are pointers

initPerceusCxt :: GrinDefinition -> PerceusCxt
initPerceusCxt def = PerceusCxt
  { pc_vars = def.gr_args
  , pc_pointers = Set.fromList def.gr_args <> gatherPointers def.gr_term }

varsLocal :: MonadReader PerceusCxt m => [Abs] -> m a -> m a
varsLocal xs = local $ \cxt -> cxt{pc_vars = cxt.pc_vars ++ xs}

deBruijnLookup :: Int -> Perceus (Maybe Abs)
deBruijnLookup n = asks $ (!!! n) . reverse . pc_vars

deBruijnOf :: Abs -> [Abs] -> Maybe Int
deBruijnOf x = genericElemIndex x . reverse

isPointer :: Abs -> Perceus Bool
isPointer x = asks $ Set.member x . pc_pointers

perceus :: GrinDefinition -> GrinDefinition
perceus def = setGrTerm term def
  where
  term = flip runReader (initPerceusCxt def) $ do
    xs <- fvTerm def.gr_term
    let (gamma, xs_drop) = first Set.fromList $ partition (`Set.member` xs) def.gr_args
    t' <- perceusTerm (Context mempty gamma) def.gr_term
    let drops = map (Drop . fromMaybe __IMPOSSIBLE__ . flip deBruijnOf def.gr_args) xs_drop
    pure $ foldr BindEmpty t' drops

-- • Cnat contains only an integer (non-pointer)
-- • The tag of a variable node is always an integer (non-pointer)
-- • Both operands and result of primitive are stored in registers (non-pointers)
-- • Currently, unit never binds pointers (due to left unit law transformation), however this maybe change with CSE.
gatherPointers :: Term -> Set Abs
gatherPointers (FetchOffset _ offset `Bind` LAltVar x t) | offset >= 2 = Set.insert x (gatherPointers t)
gatherPointers (Store _ _ `Bind` LAltVar x t) = Set.insert x (gatherPointers t)
gatherPointers (App Def{} _ `Bind` alt) =
  case alt of
    LAltVar _ t -> gatherPointers t
    LAltConstantNode tag _ xs t
     | tag == natTag -> gatherPointers t
     | otherwise -> Set.fromList xs <> gatherPointers t
    LAltVariableNode _ _ xs t -> Set.fromList xs <> gatherPointers t
    LAltEmpty t -> gatherPointers t
gatherPointers (t1 `Bind` (snd . splitLalt -> t2)) = on (<>) gatherPointers t1 t2
gatherPointers (Case _ t alts) = gatherPointers t <> foldMap step alts
  where
  step (snd . splitCalt -> t) = gatherPointers t
gatherPointers _ = mempty

type Perceus a = Reader PerceusCxt a

-- Δ | Γ ⊢ t ⟿  t′
-- TODO maybe add a pass that collect all the pointers
perceusTerm :: Context -> Term -> Perceus Term
perceusTerm c (App f vs) = do
  let vs' = List1.fromListSafe __IMPOSSIBLE__ vs
  cs <- splitContext c vs'
  dups <- fold <$> List1.zipWithM perceusVal cs vs'
  pure (foldr BindEmpty (App f vs) dups)

perceusTerm c (t1 `Bind` (splitLaltWithVars -> (mkAlt, t2, xs))) = do
  xs_ptr <- filterM isPointer xs

  fvs <- varsLocal xs (fvTerm t2)
  let (xs_new, xs_drop) = first Set.fromList $ partition (`Set.member` fvs) xs_ptr

  -- Γ ∩ (fv(t₂) - xs)
  let gamma2 = Set.intersection c.gamma (fvs Set.\\ xs_new)

  -- Δ, Γ₂ | Γ - Γ₂ ⊢ t₁ ⟿  t₁′
  let context1 = Context (c.delta <> gamma2) (c.gamma Set.\\ gamma2)
  t1' <- perceusTerm context1 t1

  -- Δ | Γ₂, xs ⊢ t₂ ⟿  t₂′
  let context2 = Context c.delta (gamma2 <> xs_new)
  t2' <- varsLocal xs (perceusTerm context2 t2)

  let drops = map (\x -> Drop $ fromMaybe __IMPOSSIBLE__ $ deBruijnOf x xs) xs_drop
  pure $ t1' `Bind` mkAlt (foldr BindEmpty t2' drops)

perceusTerm c (Case (Var n) t alts) = do
  t' <- step t
  alts' <- mapM (\(splitCalt -> (mkAlt, t)) -> mkAlt <$> step t) alts
  pure (Case (Var n) t' alts')
  where
  -- Δ | Γᵢ ⊢ tᵢ ⟿  drop Γᵢ′ ; λ () → tᵢ′
  step t = do
      x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
      unless (Set.member x c.gamma) __IMPOSSIBLE__
      let gamma = Set.delete x c.gamma

      -- Γᵢ = Γ ∩ fv(tᵢ)
      gammai <- Set.intersection gamma <$> fvTerm t
      t' <- perceusTerm (Context c.delta gammai) t

      -- Γᵢ′ = Γ - Γᵢ
      let gammai' = gamma Set.\\ gammai

      xs <- asks pc_vars
      let drops = Set.map (\x -> Drop $ fromMaybe __IMPOSSIBLE__ $ deBruijnOf x xs) gammai'

      pure (foldr BindEmpty t' drops)

perceusTerm c (Update tag n v) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  -- x must be pointer
  unlessM (isPointer x) __IMPOSSIBLE__

  gamma2 <- Set.intersection c.gamma <$> fvVal v
  foldr BindEmpty (Update tag n v) <$> perceusVal (Context c.delta gamma2) v

perceusTerm c (Unit v) = foldr BindEmpty (Unit v) <$> perceusVal c v
perceusTerm c (Store loc v) = foldr BindEmpty (Store loc v) <$> perceusVal c v
perceusTerm c (FetchOffset n offset) = foldr BindEmpty (FetchOffset n offset) <$> perceusVal c (Var n)
perceusTerm _ (Error e) = pure (Error e)

perceusTerm _ Case{} = __IMPOSSIBLE__
perceusTerm _ Fetch{} = __IMPOSSIBLE__
perceusTerm _ Decref{} = __IMPOSSIBLE__
perceusTerm _ Dup{} = __IMPOSSIBLE__

type Dups = [Term]

perceusVal :: Context -> Val -> Perceus Dups
perceusVal c (Var n) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup n
  p <- isPointer x
  let not_ptr = boolToMaybe (not p) []
      svar = boolToMaybe (p && c.gamma == Set.singleton x) []
      svar_dup = boolToMaybe (p && Set.member x c.delta) [Dup n]
  pure $ fromMaybe __IMPOSSIBLE__ (not_ptr <|> svar <|> svar_dup)

perceusVal _ (ConstantNode _ _ []) = pure []
perceusVal c (ConstantNode _ _ (v : vs)) = do
    contexts <- splitContext c (v :| vs)
    fold <$> List1.zipWithM perceusVal contexts (v :| vs)

perceusVal _ (VariableNode _ _ []) = pure []
perceusVal c (VariableNode n _ (v : vs)) = do
    contexts <- splitContext c (Var n <| v :| vs)
    fold <$> List1.zipWithM perceusVal contexts (v :| vs)

perceusVal _ _ = pure []

-- | Split context
splitContext :: Context -> List1 Val -> Perceus (List1 Context)
splitContext c vs = list1scanr step (Context c.delta) <$> gammas
  where
  step gamma c = Context (c.delta <> c.gamma) gamma

  -- Γᵢ,‥, Γₙ
  gammas = List1.scanr1 step <$> mapM fvVal vs
    where
    step fv after = Set.difference c.gamma after `Set.intersection` fv

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
  term <- FetchOffset 0 1 `bindVarR` caseRc
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
  caseRc = Case (Var 0) (Decref 0) . singleton . CAltLit (LitNat 1) <$> unique

  unique :: mf Term
  unique =
    FetchOffset 1 0 `bindVarR`
    Case (Var 0) unreachable <$> mapM mkAlt tags

  -- TODO big layout
  mkAlt :: Tag -> mf CAlt
  mkAlt tag | tag == natTag = pure $ CAltTag tag (free 2)
  mkAlt tag = CAltTag tag <$> foldrM dropChild (free 2) (take arity [2 .. ])
    where
    arity = tagArity tag
    dropChild offset t =
      FetchOffset 2 offset `bindVar`
      Drop 0               `BindEmpty`
      raise 1 t




{- Maybe remove. Would be nice to ensure invariants better than the current approach.

deltaLens :: Lens' Context (Set Abs)
deltaLens f c = f c.delta <&> \delta -> c{delta = delta}

gammaLens :: Lens' Context (Set Abs)
gammaLens f c = f c.gamma <&> \gamma -> c{gamma = gamma}

contextMember :: Abs -> Context -> Bool
contextMember x c = on (||) (Set.member x) (c ^. deltaLens) (c ^. gammaLens)

deltaInsert :: Abs -> Context -> Context
deltaInsert x c | contextMember x c = c
                | otherwise = over deltaLens (Set.insert x) c

-- Precondition: x ∈ fv(t)
gammaInsert :: Abs -> Context -> Context
gammaInsert x c | contextMember x c = c
                | otherwise = over gammaLens (Set.insert x) c

-}
