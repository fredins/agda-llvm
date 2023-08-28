
treelessToGrin :: MonadFresh Int mf => [TreelessDefinition] -> mf [GrinDefinition]
treelessToGrin defs =
  forM defs $ \def -> do
    gr_term <- runReaderT (termToGrinR def.tl_term) (initGrinGenCxt def primitives)
    gr_args <- replicateM def.tl_arity freshAbs
    gr_return <- boolToMaybe (not def.tl_isMain) <$> freshAbs
    pure $ GrinDefinition
      { gr_name = def.tl_name
      , gr_isMain = def.tl_isMain
      , gr_primitive = def.tl_primitive
      , gr_arity = def.tl_arity
      , gr_type = Just def.tl_type
      , gr_term = gr_term
      , gr_args = gr_args
      , gr_return = gr_return
      }
  where
    primitives = mapMaybe (\def -> (, def.tl_name) <$> def.tl_primitive) defs

termToGrinR :: MonadFresh Int mf => TTerm -> GrinGen mf Term
termToGrinR (TCase n CaseInfo{caseType} t alts) =
  caseMaybeM (applyEvaluatedOffset n)
    mkEval
    reuseEval
  where
    mkEval = do
      alts' <- localEvaluatedOffset n (mapM altToGrin alts)
      t' <- localEvaluatedOffset n (termToGrinR t)
      eval n `bind` Case (Var 0) t' alts'

    reuseEval n' = do
      alts' <- mapM altToGrin alts
      t' <- termToGrinR t
      pure (Case (Var n') t' alts')

    bind = case caseType of
      CTNat    -> bindCnat
      CTData{} -> bindVar
      _        -> __IMPOSSIBLE__

    -- | 𝓡 [x + y] = eval 1 ; λ Cnat x0 →
    --               eval 1 ; λ Cnat x1 →
    --               add 1 0 ; λ x2 →
    --               unit 0
termToGrinR (TApp (TPrim prim) vs) =  evaluateValues (App (Prim prim)) vs
  where
  -- FIXME untested
  evaluateValues :: MonadFresh Int mf => ([Val] -> Term) -> [TTerm] -> GrinGen mf Term
  evaluateValues mkT vs = do
    (f, vs') <- forAccumM id vs $ \f v -> mkEval v <&> either (f, ) (\f' -> (f' . f, Var 0))
    (f . pure . mkT) vs'

  mkEval :: (MonadFresh Int m, MonadReader GrinGenCxt m) => TTerm -> m (Either Val (m Term -> m Term))
  mkEval (TVar n) =
    caseMaybeM (applyEvaluatedOffset n)
      (Right <$> do
        x <- freshAbs
        pure (fmap (Bind (eval n) . LAltVar x) . localEvaluatedOffset n))
      (pure . Left . Var)
  mkEval (TLit lit) = pure (Left (Lit lit))
  mkEval _ = __IMPOSSIBLE__

-- | 𝓡 [foo x y] = foo x y
termToGrinR (TApp (TDef q) vs) = App (Def (prettyShow q)) <$> mapM operandToGrin vs

-- 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
termToGrinR (TApp (TCon q) vs) =
  caseList vs
    (pure (Unit (Tag (FTag name 0))))
    (\v vs -> Unit . constantCNode name <$> mapM operandToGrin  (v :| vs))
  where
  name = prettyShow q

-- 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ x → 𝓡 [t2]
termToGrinR (TLet t1 t2) = termToGrinC t1 `bindVarM` termToGrinR t2
termToGrinR (TCon q) = pure (Unit (Tag (CTag (prettyShow q) 0)))
termToGrinR (TLit lit) = pure (Unit (ConstantNode natTag (List1.singleton (Lit lit))))
termToGrinR (TError TUnreachable) = pure unreachable

termToGrinR t = error $ "TODO: " ++ take 40 (show t)

termToGrinC (TApp (TPrim prim) vs) = do
  name <- asks (fromMaybe __IMPOSSIBLE__ . lookup prim . primitives)
  loc <- freshLoc
  appToGrinC (Store loc . ConstantNode (FTag name (length vs))) vs
termToGrinC (TApp (TDef q) vs) = do
  loc <- freshLoc
  appToGrinC (Store loc . ConstantNode (FTag (prettyShow q) (length vs))) vs
termToGrinC (TApp (TCon q) vs) = do
  loc <- freshLoc
  appToGrinC (Store loc . ConstantNode (CTag (prettyShow q) (length vs))) vs
termToGrinC t = error $ "TODO: " ++ take 40 (show t)

--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
appToGrinC :: MonadFresh Int mf => (List1 Val -> Term) -> [TTerm] -> mf Term
appToGrinC mkT vs = undefined
  where

  mkStore (TLit lit) = do
    let f t = store (cnat (Lit lit)) `bindVarL` t
    undefined
  mkStore (TVar n) = pure (Var n)
  mkStore _ = __IMPOSSIBLE__




altToGrin :: MonadFresh Int mf => TAlt -> GrinGen mf CAlt
altToGrin (TACon q arity t) = do
    t' <- localEvaluatedNoOffsets arity (termToGrinR t)
    caltConstantNode tag t'
  where
   tag = CTag (prettyShow q) arity
altToGrin (TALit lit t) = CAltLit lit <$> termToGrinR t
altToGrin _ = __IMPOSSIBLE__

operandToGrin :: MonadFresh Int mf => TTerm -> GrinGen mf Val
operandToGrin (TVar n)   = applyEvaluatedOffset n <&> maybe (Var n) Var
operandToGrin (TLit lit) = pure (Lit lit)
operandToGrin _          = __IMPOSSIBLE__

type GrinGen mf a = ReaderT GrinGenCxt mf a

data GrinGenCxt = GrinGenCxt
  { primitives       :: [(TPrim, String)]
  , definition       :: TreelessDefinition
  , evaluatedOffsets :: [Maybe (Int -> Int)]
  }

initGrinGenCxt :: TreelessDefinition -> [(TPrim, String)] -> GrinGenCxt
initGrinGenCxt def primitives = GrinGenCxt
  { primitives = primitives
  , definition = def
  , evaluatedOffsets = replicate def.tl_arity Nothing
  }

applyEvaluatedOffset :: MonadReader GrinGenCxt m => Int -> m (Maybe Int)
applyEvaluatedOffset n = asks (fmap ($ n) . fromMaybe __IMPOSSIBLE__ . (!!! n) . evaluatedOffsets)

localEvaluatedOffset :: MonadReader GrinGenCxt m => Int -> m a -> m a
localEvaluatedOffset n =
  let offset n' = n' - (n + 1) in
  local $ \cxt -> cxt{evaluatedOffsets = Nothing : updateAt n (const (Just offset)) (cxt.evaluatedOffsets)}

localEvaluatedNoOffset :: MonadReader GrinGenCxt m => m a -> m a
localEvaluatedNoOffset = localEvaluatedNoOffsets 1

localEvaluatedNoOffsets :: MonadReader GrinGenCxt m => Int -> m a -> m a
localEvaluatedNoOffsets n =
  local $ \cxt -> cxt{evaluatedOffsets = replicate n Nothing ++ cxt.evaluatedOffsets}

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var
