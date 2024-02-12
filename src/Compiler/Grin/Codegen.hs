
module Compiler.Grin.Codegen (treelessToGrin) where

import           Control.Applicative                 (liftA2)
import           Control.Monad                       (replicateM)
import           Control.Monad.Reader                (MonadReader,
                                                      ReaderT (runReaderT),
                                                      asks, local)
import           Data.Foldable                       (foldrM)
import           Data.List                           (isSuffixOf, mapAccumR,
                                                      singleton)


import           Agda.Compiler.Backend               hiding (Prim)
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal                 (Literal (LitNat))
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Function                 (applyWhen)
import           Agda.Utils.Impossible               (__IMPOSSIBLE__)
import           Agda.Utils.List                     (caseList, snoc, updateAt)
import           Agda.Utils.List1                    (List1, pattern (:|))
import qualified Agda.Utils.List1                    as List1
import           Agda.Utils.Maybe                    (boolToMaybe)
import           Agda.Utils.Monad                    (mapMaybeM)

import           Compiler.Grin.Grin
import           Compiler.Treeless.TreelessTransform
import qualified Utils.List1                         as List1

-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • treelessToGrin to Grin should only take one @TreelessDefinition@ so it can support
--   seperate compilation. To do this, we need to solve the primitives issue. The plan
--   is to to modify the current treeless implementation:
--
--   >>> current
--
--   RTE.hs
--     addInt :: Integer -> Integer -> Integer
--     addInt = (+)
--
--   file.hs
--     main = ... addInt ...
--
--   >>> new
--
--   file.hs
--     main = ... (+) ...    perhaps with type application (+) @Integer or type annotation ((+) :: Integer -> Integer -> Integer)
--
--
--   Implementation
--
--    Treeless/ToTreeless.hs needs to be modified so a compiler pass can accepts a QName too.
--
--      - compilerPass :: String -> Int -> String -> (EvaluationStrategy -> TTerm -> TCM TTerm) -> Pipeline
--      + compilerPass :: String -> Int -> String -> (EvaluationStrategy -> QName -> TTerm -> TCM TTerm) -> Pipeline
--
--    Treeless/Builtin.hs replace the the definition body instead of the callsite.
--
--      _+_ : Nat → Nat → Nat
--      zero  + m = m
--      suc n + m = suc (n + m)
--
--      {-# BUILTIN NATPLUS _+_ #-}
--
--      main = 1 + 2
--
--      >>> translateBuiltins
--
--      _+_ : Nat → Nat → Nat
--      n + m = PAdd n m
--
--      main = 1 + 2
--
--      >>> MAlonzo
--
--      _+_ = (+)
--
--      main = Agda.Builtin.Nat._+_ 1 2
--
--
--
--      Changes:
--
--      + -- Example: Agda.Builtin.Nat._+_ returns PAdd (How are arguments dealt with?)
--      + lookupBuiltin : QName -> TCM (Maybe TTerm)
--      + lookupBuiltin q = ...
--
--      - translateBuiltins :: TTerm -> TCM TTerm
--      + translateBuiltins :: QName -> TTerm -> TCM TTerm
--      - translateBuiltins q t = do
--      + translateBuiltins q t = do
--          kit <- builtinKit
--      -   return $ transform kit t
 --     +   fromMaybe t <$> lookupBuiltin q
--
--
--    MAlonzo/Primitives.hs we do not need
--
--      treelessPrimName :: TPrim -> String
--      treelessPrimName p =
--        case p of
--      -   PAdd    -> "addInt"
--      +   PAdd    -> "(+) @Integer"
--      -   PAdd64  -> "addInt64"
--      +   PAdd64  -> "(+) @Word64"
--
--    Questions:
--      1. Is @PAdd64@ unused?
--      2. Why do we create wrappers? E.g. @intAdd@ instead of @(+)@
--      3. Why are the types specialized? @intAdd :: Integer -> Integer -> Integer@ instead of @Num a => a -> a -> a@
--      4. Builtin in Treeless but Primitive in MAlonzo. Why?
--
-- • Fix super ugly code
-- • Need to deal with erased arguments and parameters
-- • Reuse evaluated variables (partially done)
-- • Fill in rest of the patterns

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions
-- • Saturated constructors

-- TODO: remember to raise when ever we create a new abstraction that doesn't exist in treeless
treelessToGrin :: MonadFresh Int mf => TreelessDefinition -> mf GrinDefinition
treelessToGrin def = do
    -- traceM ("\n" ++ def.tl_name ++ " final treeless:\n" ++ prettyShow def)
    gr_term <- runReaderT (termToGrinR def.tl_term) (initGrinGenCxt def)
    -- traceM ("\n" ++ def.tl_name ++ " to grin:\n" ++ prettyShow gr_term)
    gr_args <- replicateM def.tl_arity freshAbs
    gr_return <- boolToMaybe (not def.tl_isMain) <$> freshAbs

    pure $ GrinDefinition
      { gr_name = if "main" `isSuffixOf` def.tl_name then "main" else def.tl_name
      , gr_isMain = def.tl_isMain
      , gr_primitive = def.tl_primitive
      , gr_arity = def.tl_arity
      , gr_type = Just def.tl_type
      , gr_term = gr_term
      , gr_args = gr_args
      , gr_return = gr_return
      }

termToGrinR :: forall mf. MonadFresh Int mf => TTerm -> GrinGen mf Term
termToGrinR (TCase n CaseInfo{caseType} t alts) =
  -- caseMaybeM (applyEvaluatedOffset n)
    mkEval'
    -- reuseEval
  where
    mkEval' = case caseType of
      -- Do not keep track evaluated naturals because it causes trouble
      -- with unboxed/boxed types.
      -- TODO keep track when strictness/demand-analysis and general unboxing is
      --      implemented.
      CTNat -> do
        (t', alts') <-
          localEvaluatedNoOffset $
          liftA2 (,) (termToGrinR $ raise 1 t) (mapM altToGrin $ raise 1 alts)
        eval n `bindCnat` Case (Var 0) t' alts'
      CTData{} -> do
        (t', alts') <-
          localEvaluatedOffset n $
          localEvaluatedNoOffset $
          liftA2 (,) (termToGrinR $ raise 1 t) (mapM altToGrin $ raise 1 alts)
        eval n `bindVar` Case (Var 0) t' alts'
      _ -> __IMPOSSIBLE__


    reuseEval n' = do
      alts' <- mapM altToGrin alts
      t' <- termToGrinR t
      pure (Case (Var n') t' alts')

-- primForce in R scheme is unnecessary
-- seq x (f x)
termToGrinR (TApp (TPrim PSeq) (_ : f : xs)) = termToGrinR $ mkTApp f xs

-- | 𝓡 [x + y] = eval 1 ; λ Cnat x0 →
--               eval 1 ; λ Cnat x1 →
--               add 1 0 ; λ x2 →
--               unit 0
termToGrinR (TApp (TPrim prim) ts) = do
  t <- forceValues (App (Prim prim)) ts
  t `bindVar` Unit (cnat $ Var 0)

  where
  forceValues :: MonadFresh Int mf => ([Val] -> Term) -> [TTerm] -> GrinGen mf Term
  forceValues mkT vs = do
    (mevals, vs') <- foldrM step (Nothing, []) vs
    case mevals of
      Just evals ->  bindEnd evals <$> laltConstantNode natTag (mkT vs')
      Nothing    -> pure (mkT vs')
    where
    -- TODO add evaluatedOffsets
    step :: (MonadReader GrinGenCxt m, MonadFresh Int m) => TTerm -> (Maybe Term, [Val]) -> m (Maybe Term, [Val])
    step (TLit lit) (mevals, vs) = pure (mevals, Lit lit : vs)
    step (TVar n) (mevals, vs) =
      -- TODO: need distinguish between boxed and unboxed
      {- applyEvaluatedOffset n >>= \case
        Just n' -> pure (mevals, Var n' : vs)
        Nothing -> -} do
          evals' <- case mevals of
            Just evals -> eval n `bindCnat` raise 1 evals
            Nothing    -> pure (eval n)

          pure (Just evals', raise 1 vs `snoc` Var 0)
    step _ _ = __IMPOSSIBLE__

-- | 𝓡 [foo x y] = foo x y
termToGrinR (TApp (TDef q) vs) = do
  let call = App (Def (prettyShow q)) <$> mapMaybeM operandToGrin vs
  shortName <- asks $ List1.last . List1.splitOnDots . tl_name . definition
  returning <- asks returning
  applyWhen (shortName == "main" && returning) (`bindCnatL` printf 0) call


-- TODO think of a nicer way to handle handle functions that don't return
--      a node (either an unboxed value or `()`)
termToGrinR (TLit lit) = do
  shortName <- asks $ List1.last . List1.splitOnDots . tl_name . definition
  if shortName == "main" then
    case lit of
      LitNat n -> pure $ printf (fromInteger n)
      _        -> __IMPOSSIBLE__
  else
    pure $ Unit $ cnat (Lit lit)


-- 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
termToGrinR (TApp (TCon q) vs) =
  caseList vs
    (pure (Unit (Tag (FTag name 0))))
    (\v vs -> Unit . constantCNode name  <$> mapMaybeM operandToGrin  (v : vs))
  where
  name = prettyShow q


-- Forcing and argument via the identity function
termToGrinR (TLet (TApp (TPrim PSeq) [TVar n, TVar n']) t) | n == n' =
  localReturning False (termToGrinR $ TVar n) `bindVarM`
  store (Var 0) `bindVarM`
  (raiseFrom 1 1 <$> termToGrinR t)

-- TODO generalize
termToGrinR (TLet (TApp (TPrim PSeq) (_ : TApp f xs : ys)) t) =
  localReturning False (termToGrinR $ TApp f $ xs ++ ys) `bindVarM`
  store (Var 0) `bindVarM`
  (raiseFrom 1 1 <$> termToGrinR t)
  -- app <- termToGrinR $ mkTApp f xs
  -- t' <- raiseFrom 1 1 <$> termToGrinR t
  -- app `bindCnatR` store (cnat $ Var 0) `bindVarL` t'

-- 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ x → 𝓡 [t2]
termToGrinR (TLet t1 t2) = termToGrinC t1 `bindVarM` localEvaluatedNoOffset (termToGrinR t2)
-- Always return a node
termToGrinR (TCon q) = pure (Unit (constantCNode (prettyShow q) []))
termToGrinR (TError TUnreachable) = pure Unreachable
termToGrinR (TVar n) = maybe (eval n) (Unit . Var) <$> applyEvaluatedOffset n

termToGrinR t = error $ "TODO R scheme: " ++ prettyShow t


termToGrinC (TApp f []) = error "TODO CAF"

--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)-
--
-- TODO this step should be unnecessary due to simplifyApp
termToGrinC (TApp (TDef q) (v : vs))
  | Just (v' :| vs') <- allTVars1 (v :| vs) = do
    loc <- freshLoc
    pure $ Store loc $ constantFNode (prettyShow q) (v' : vs')

termToGrinC (TApp (TCon q) (v : vs))
  | Just (v' :| vs') <- allTVars1 (v :| vs) = do
  loc <- freshLoc
  pure $ Store loc $ constantCNode (prettyShow q) (v' : vs')




  -- termToGrinR (TApp f $ xs ++ ys) `bindCnatM`
  -- store (cnat $ Var 0) `bindVarM`
  -- (raiseFrom 1 1 <$> termToGrinR t)

-- termToGrinC (TApp (TPrim PSeq) vs) = undefined

-- termToGrinC (TApp (TPrim prim) (v : vs))
--   | Just (v' :| vs') <- allTVars1 (v :| vs) = do
--   name <- asks (fromMaybe __IMPOSSIBLE__ . lookup prim . primitives)
--   loc <- freshLoc
--   pure $ Store loc $ constantFNode name (v' : vs')

termToGrinC (TLit (LitNat n)) = do
  loc <- freshLoc
  pure $ Store loc (ConstantNode natTag [mkLit n])

termToGrinC (TCon q) = do
  loc <- freshLoc
  pure $ Store loc (constantCNode (prettyShow q) [])


-- TODO this probably works but currently we normalise all lets in TreelessTransform which is a bit unnecessary
--      as everything will be normalised again after eval inlining
-- termToGrinC (TLet t1 t2) = termToGrinC t1 `bindVarM` localEvaluatedNoOffset (termToGrinC t2)

termToGrinC (TApp (TVar n) xs) = undefined
termToGrinC t = error $ "TODO C scheme: " ++ take 80 (show t)

allTVars1 :: List1 TTerm -> Maybe (List1 Val)
allTVars1 (TVar n :| vs) = (Var n :|) <$> allTVars vs
allTVars1 _              = Nothing

allTVars :: [TTerm] -> Maybe [Val]
allTVars (TVar n : vs) = (Var n :) <$> allTVars vs
allTVars []            = Just []
allTVars _             = Nothing

appToGrinC :: MonadFresh Int mf => (List1 Val -> Term) -> List1 TTerm -> mf Term
appToGrinC mkT vs = foldrM step (mkT vs') stores
  where
  step s t = do
    loc <- freshLoc
    s loc `bindVar` t

  ((_, stores), vs') = mapAccumR mkStore (0, []) vs

  mkStore (n, stores) (TLit lit) = ((n + 1, store : stores), Var n)
    where
    store loc = Store loc (ConstantNode natTag [Lit lit])
  mkStore (n, stores) (TVar m) = ((n, stores), Var (m + n))
  mkStore _ t = error $ "MKSTORE: " ++ show t


altToGrin :: MonadFresh Int mf => TAlt -> GrinGen mf CAlt
altToGrin (TACon q arity t) = do
    t' <- localEvaluatedNoOffsets arity (termToGrinR t)
    caltConstantNode tag t'
  where
   tag = CTag (prettyShow q) arity
altToGrin (TALit lit t) = CAltLit lit <$> termToGrinR t
altToGrin (TAGuard t1 t2) = liftA2 CAltGuard (termToGrinR t1) (termToGrinR t2)

-- Erased parameter return Nothing
-- FIXME function definition is not modified yer...
operandToGrin :: MonadFresh Int mf => TTerm -> GrinGen mf (Maybe Val)
-- ugly!!!
operandToGrin (TVar n) = Just <$> (maybe (Var n) Var <$> applyEvaluatedOffset n)
operandToGrin _        = pure Nothing
-- Literals should not be operands!!
--operandToGrin (TLit lit) = pure (Lit lit)

type GrinGen mf a = ReaderT GrinGenCxt mf a

data GrinGenCxt = GrinGenCxt
  { definition       :: TreelessDefinition
  , evaluatedOffsets :: [Maybe (Int -> Int)]
  , returning        :: Bool
  }

initGrinGenCxt :: TreelessDefinition -> GrinGenCxt
initGrinGenCxt def = GrinGenCxt
  { definition = def
  , evaluatedOffsets = replicate def.tl_arity Nothing
  , returning = True
  }

-- FIXME
applyEvaluatedOffset :: MonadReader GrinGenCxt m => Int -> m (Maybe Int)
applyEvaluatedOffset _ = pure Nothing
-- applyEvaluatedOffset :: MonadReader GrinGenCxt m => Int -> m (Maybe Int)
-- applyEvaluatedOffset n = asks $ fmap ($ n) . join . (!!! n) . evaluatedOffsets

localEvaluatedOffset :: MonadReader GrinGenCxt m => Int -> m a -> m a
localEvaluatedOffset n =
  let offset n' = n' - (n + 1) in
  local $ \cxt -> cxt{evaluatedOffsets = updateAt n (const (Just offset)) (cxt.evaluatedOffsets)}

localEvaluatedNoOffset :: MonadReader GrinGenCxt m => m a -> m a
localEvaluatedNoOffset = localEvaluatedNoOffsets 1

localEvaluatedNoOffsets :: MonadReader GrinGenCxt m => Int -> m a -> m a
localEvaluatedNoOffsets n =
  local $ \cxt -> cxt{evaluatedOffsets = replicate n Nothing ++ cxt.evaluatedOffsets}


localReturning :: MonadReader GrinGenCxt m => Bool -> m a -> m a
localReturning x = local $ \cxt -> cxt{returning = x}

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var
