{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Agda.Llvm.Compiler (module Agda.Llvm.Compiler) where

import           Control.DeepSeq                (NFData)
import           Control.Monad                  (forM, replicateM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State            (StateT, evalStateT, gets)
import           Data.Foldable                  (foldrM)
import           Data.Function                  (on)
import           Data.List                      (intercalate, singleton)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           GHC.Generics                   (Generic)
import           GHC.IO                         (unsafePerformIO)
import           Prelude                        hiding ((!!))

import           Agda.Compiler.Backend          hiding (Prim, initEnv)
import           Agda.Interaction.Options
import           Agda.Llvm.Grin
import           Agda.Llvm.GrinInterpreter      (interpretGrin)
import           Agda.Llvm.GrinTransformations
import           Agda.Llvm.HeapPointsTo
import           Agda.Llvm.TreelessTransform
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1               (pattern (:|))
import           Agda.Utils.Maybe


llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LlvmOptions LlvmEnv LlvmModuleEnv LlvmModule (Maybe TreelessDefinition)
llvmBackend' = Backend'
  { backendName           = "LLVM"
  , backendVersion        = Nothing
  , options               = defaultLlvmOptions
  , commandLineFlags      = llvmCommandLineFlags
  , isEnabled             = flagLlvmCompile
  , preCompile            = llvmPreCompile
  , postCompile           = llvmPostCompile
  , preModule             = \_ _ _ -> pure $ pure $ Recompile LlvmModuleEnv
  , postModule            = llvmPostModule
  , compileDef            = llvmCompileDef
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ pure True
  }

newtype LlvmOptions = LlvmOptions
  { flagLlvmCompile :: Bool
  } deriving (Generic, NFData)

data LlvmEnv = LlvmEnv {}

data LlvmModuleEnv = LlvmModuleEnv
newtype LlvmModule = LlvmModule [TreelessDefinition]

defaultLlvmOptions :: LlvmOptions
defaultLlvmOptions = LlvmOptions
  { flagLlvmCompile = False
  }

llvmCommandLineFlags :: [OptDescr (Flag LlvmOptions)]
llvmCommandLineFlags =
    [ Option []  ["llvm"] (NoArg enable)
      "compile program using the LLVM backend"
    ]
  where
    enable o = pure o{flagLlvmCompile = True}

llvmPreCompile :: LlvmOptions -> TCM LlvmEnv
llvmPreCompile _ = pure LlvmEnv

-- TODO need to filter unreachable functions
llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM (Maybe TreelessDefinition)

llvmCompileDef _ _ = definitionToTreeless

llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [Maybe TreelessDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ _ defs =
  pure $ LlvmModule $ catMaybes defs

llvmPostCompile :: LlvmEnv
                -> IsMain
                -> Map TopLevelModuleName LlvmModule
                -> TCM ()
llvmPostCompile _ _ mods = do

  let defs_treeless = concatMap (\(LlvmModule xs) -> xs) (Map.elems mods)
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Treeless"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_treeless

  defs_grin <- treelessToGrin defs_treeless
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * GRIN"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_grin

  let (absCxtEqs, absCxt, share) = heapPointsTo defs_grin
  let tagInfo_pointsTo = initTagInfo absCxt
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Heap points-to analysis"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn "Equations"
    putStrLn $ prettyShow absCxtEqs
    putStrLn ""
    putStrLn $ prettyShow absCxt
    putStrLn $ "\nSharing Heap: " ++ prettyShow share
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_pointsTo

  (defs_inlineEval, tagInfo_inlineEval) <- inlineEval defs_grin absCxt tagInfo_pointsTo
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Inlining Eval"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_inlineEval
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_inlineEval

  res_inlineEval <- interpretGrin defs_inlineEval
  liftIO $ putStrLn $ "\nResult: " ++ show res_inlineEval

  let defs_normalize = map (updateGrTerm normalise) defs_inlineEval
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalize

  res_normalise <- interpretGrin defs_normalize
  liftIO $ putStrLn $ "\nResult: " ++ show res_normalise

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_normalize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  res_leftUnitLaw <- interpretGrin defs_leftUnitLaw
  liftIO $ putStrLn $ "\nResult: " ++ show res_leftUnitLaw

  let defs_specializeUpdate = map (updateGrTerm specializeUpdate) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Specialize Update"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_specializeUpdate

  res_specializeUpdate <- interpretGrin defs_specializeUpdate
  liftIO $ putStrLn $ "\nResult: " ++ show res_specializeUpdate

  defs_vectorize <- mapM (lensGrTerm $ vectorize tagInfo_inlineEval) defs_specializeUpdate
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Vectorization"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_vectorize

  res_vectorize <- interpretGrin defs_vectorize
  liftIO $ putStrLn $ "\nResult: " ++ show res_vectorize

  let defs_simplifyCase = map (updateGrTerm simplifyCase) defs_vectorize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Simplify case"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_simplifyCase

  res_simplifyCase <- interpretGrin defs_simplifyCase
  liftIO $ putStrLn $ "\nResult: " ++ show res_simplifyCase

  let defs_splitFetch = map (updateGrTerm splitFetch) defs_simplifyCase
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Split fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_splitFetch

  res_splitFetch <- interpretGrin defs_splitFetch
  liftIO $ putStrLn $ "\nResult: " ++ show res_splitFetch

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_splitFetch
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  res_leftUnitLaw <- interpretGrin defs_leftUnitLaw
  liftIO $ putStrLn $ "\nResult: " ++ show res_leftUnitLaw

  defs_rightHoistFetch <- mapM (lensGrTerm rightHoistFetch) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Right hoist fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_rightHoistFetch

  res_rightHoistFetch <- interpretGrin defs_rightHoistFetch
  liftIO $ putStrLn $ "\nResult: " ++ show res_rightHoistFetch

  defs_introduceRegisters  <- mapM (lensGrTerm introduceRegisters) defs_rightHoistFetch
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Introduce Registers"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_introduceRegisters

  res_introduceRegisters <- interpretGrin defs_introduceRegisters
  liftIO $ putStrLn $ "\nResult: " ++ show res_introduceRegisters

-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • Refactor (use Reader instead of State)
-- • Use bind combinators
-- • Reuse evaluated variables
-- • Fill in rest of the patterns

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors
treelessToGrin :: [TreelessDefinition] -> TCM [GrinDefinition]
treelessToGrin defs = forM defs $ \def -> do
  gr_term <- evalStateT (rScheme def.tl_term) $
           initGEnv def.tl_arity def.tl_isMain
  gr_args <- replicateM def.tl_arity freshAbs
  gr_return <- boolToMaybe (not def.tl_isMain) <$> freshAbs
  pure $ GrinDefinition
    { gr_name = def.tl_name
    , gr_isMain = def.tl_isMain
    , gr_arity = def.tl_arity
    , gr_type = Just def.tl_type
    , gr_term = gr_term
    , gr_args = gr_args
    , gr_return = gr_return
    }

rScheme :: TTerm -> G Term
rScheme (TCase n CaseInfo{caseType=CTNat} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> laltConstantNode natTag (Case (Var 0) def' alts')

rScheme (TCase n CaseInfo{caseType=CTData _} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> laltVar (Case (Var 0) def' alts')

-- | 𝓡 [_[]_] = unit (C_[]_)
rScheme (TCon q) = pure $ Unit $ Tag tag where
  tag = CTag{tCon = prettyShow q, tArity = 0}
rScheme (TLit lit) = pure $ Unit $ ConstantNode natTag $ Lit lit :| []
rScheme (TError TUnreachable) = pure $ Error TUnreachable

rScheme (TApp t as) = do
    isMain <- gets isMain
    alt <- laltConstantNode natTag (printf 0)
    let res t
          | isMain = Bind t alt
          | otherwise = t
    case t of
      TPrim prim -> value <$> appPrim res prim as
      TDef q     -> value <$> appDef res q as
      TCon q     -> value <$> appCon res q as
      _          -> __IMPOSSIBLE__
  where
    -- | 𝓡 [x + y] = eval @1 ; λ Cnat #1 →
    --               eval @1 ; λ Cnat #1 →
    --               add @1 @0 ; λ #1 →
    --               unit @0
    appPrim res prim as = do
        fin <- App (Prim prim) vs `bindVar` res (Unit $ ConstantNode natTag $ Var 0 :| [])
        evals' <- foldrM (\t ts -> Bind t <$> laltConstantNode natTag ts) fin evals
        pure $ mkWithOffset (length evals) evals'
      where
        (evals, vs) =  foldr f ([], []) as

        f (TLit lit) (es, vs) = (es, Lit lit : vs)
        f (TVar n)   (es, vs) = (eval (on (-) length evals es + n - 1)  : es, Var (length es) : vs)
        f _          _        = __IMPOSSIBLE__

    -- | 𝓡 [foo x y] = foo x y
    appDef res q as = do
      let
        nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

        f (TLit lit) (ss, vs) = do
            s <- store $ ConstantNode natTag $ Lit lit :| []
            pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__

      (stores, vs) <- foldrM f ([], []) as
      let fin = res $ App (Def $ prettyShow q) vs
      stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores

      pure $ mkWithOffset nLits stores'

    -- | 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
    appCon res q as = do
        let
          nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

          f (TLit lit) (ss, vs) = do
            s <- store $ ConstantNode natTag $ Lit lit :| []
            pure (s : ss, Var (length ss) : vs)
          f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
          f _          _        = __IMPOSSIBLE__

        (stores, vs) <- foldrM f ([], []) as
        let fin = res $ Unit $ ConstantNode tag $ caseList vs __IMPOSSIBLE__ (:|)
        stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores

        pure $ mkWithOffset (length stores) stores'
      where
        tag = CTag{tCon = prettyShow q, tArity = length as}

-- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
rScheme (TLet t1 t2)
  | TApp t as <- t1 = do
    WithOffset{value=t1', offset} <- cSchemeApp t as
    t2' <- rScheme $ raiseFrom 1 offset t2
    pure $ t1' t2'

  | TLet t1 t2 <- t1 = do
    t1' <- cScheme t1
    t2' <- rScheme t2
    Bind t1' <$> laltVar t2'

rScheme t = error $ "TODO rScheme " ++ show t

aScheme :: TAlt -> G CAlt
aScheme TALit{aLit, aBody} = do
  aBody' <- rScheme aBody
  pure $ CAltLit aLit aBody'

aScheme TACon{aCon, aArity, aBody} = do
    aBody' <- rScheme aBody
    caltConstantNode tag aBody'
  where
    tag = CTag{tCon = prettyShow aCon, tArity = aArity}

aScheme alt                = error $ "TODO aScheme " ++ show alt

cScheme :: TTerm -> G Term
cScheme t = error $ "TODO cScheme " ++ show t

-- | 𝓒 [foo x y] = store (Ffoo @1 @0)
--
--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
cSchemeApp :: TTerm -> Args -> G (WithOffset (Term -> Term))
cSchemeApp t as = do
    let
        nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as
        f (TLit lit) (ss, vs) = do
          s <- store $ ConstantNode natTag $ Lit lit :| []
          pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__

    (stores, vs) <-  foldrM f ([], []) as

    -- let fin' = res $ Unit $ Node tag vs
    t <- store $ ConstantNode tag $ caseList vs __IMPOSSIBLE__ (:|)
    abs <- freshAbs
    let fin = Bind t . LAltVar abs
    stores' <- foldrM (\t ts -> (\abs -> Bind t . LAltVar abs . ts) <$> freshAbs) fin stores
    pure $ mkWithOffset nLits stores'
  where
  tag
    | TDef q <- t = FTag{tDef = prettyShow q, tArity = length as}
    | TPrim prim <- t =  FTag {tDef=primStr prim, tArity=length as}
    | otherwise = __IMPOSSIBLE__

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var

-- FIXME
primStr :: TPrim -> String
-- primStr PAdd64 = "Prim.add"
primStr PAdd = "Prim.add"
-- primStr PSub64 = "Prim.sub"
primStr PSub = "Prim.sub"
primStr p    = error $ "TODO primStr " ++ show p

data GVarInfo = GVarInfo
  { isEvaluated      :: Bool
  , evaluationOffset :: Maybe Int -- negative
  }

mkVar :: GVarInfo
mkVar = GVarInfo{isEvaluated=False, evaluationOffset=Nothing}

data GEnv = GEnv
  { gVars  :: [GVarInfo]
  , isMain :: Bool
  }

initGEnv :: Int -> Bool -> GEnv
initGEnv arity isMain = GEnv {gVars = replicate arity mkVar, isMain = isMain}

type G = StateT GEnv TCM

data WithOffset a = WithOffset
  { offset :: Int
  , value  :: a
  }

mkWithOffset :: Int -> a -> WithOffset a
mkWithOffset n a = WithOffset{offset = n, value = a}

-----------------------------------------------------------------------
-- * GRIN transformations
-----------------------------------------------------------------------




-----------------------------------------------------------------------
-- * LLVM code generation
-----------------------------------------------------------------------

-- • Turns low-level GRIN into LLVM IR
-- • Node values are represented with the type %Node = type [ 4 x i64 ].
--   The first index contains the unique tag, and the rest of the values are
--   usually pointers to other Nodes. The only exception
--   is Cnat which contains an unboxed i64 in the second index.
-- • Uses utility functions (mkNode, selᵢ, ...) to generate LLVM IR


-- TODO

-- data Cxt = Cxt
--   { gDefs      :: [GrinDefinition]
--   , builtins   :: Builtins
--   , vars       :: [VarInfo]
--     -- ^ Only variables from the GRIN program.
--     --   The integer is the corresponding Llvm variable (e.g. %4).
--   , currentDef :: GrinDefinition
--   }
--
-- initCxt :: [GrinDefinition] -> Builtins -> Cxt
-- initCxt defs builtins = Cxt
--   { gDefs = defs
--   , builtins = builtins
--   , vars = []
--   , currentDef = head defs
--   }
--
-- data VarInfo = VarInfo
--   { name :: String
--   }
--
-- data Env = Env
--   { varCount :: Int
--   }
--
-- initEnv :: Env
-- initEnv = Env {varCount = 0}
--
--
--
-- type M a = StateT Env (Reader Cxt) a
--
-- grinToLlvm :: [GrinDefinition] -> M [L.Instruction]
-- grinToLlvm defs =
--   forM defs $ \def -> do
--     put initEnv
--     let vars = take def.gr_arity $ map (VarInfo . singleton) ['a' ..]
--     local (\cxt -> cxt {vars=vars , currentDef=def}) $ defToLlvm def
--
-- defToLlvm :: GrinDefinition -> M L.Instruction
-- defToLlvm GrinDefinition{gr_name, gr_type=Just typ, gr_term, gr_arity} = do
--   (ts, t) <- bimapM (mapM typeToLlvm) typeToLlvm $ returnType typ
--   body <- termToLlvm gr_term
--
--   pure $
--     L.Define
--       L.Fastcc
--       L.Ptr
--       (L.MkVar $ prettyShow gr_name)
--       (zip (replicate gr_arity L.Ptr) $ map singleton ['a' ..])
--       body
--
-- termToLlvm :: Term -> M [L.Instruction]
-- termToLlvm (Bind t alt) = liftA2 (++) (termToLlvm t) (altToLlvm alt)
--
-- termToLlvm (Fetch (Var n)) = do
--   x <- getVar n
--   getelementptr <- setVar $
--     L.Getelementptr nodeType
--       [ (L.Ptr, L.Var x)
--       , (L.I32, llvmLit 0)
--       , (L.I32, llvmLit 0)
--       ]
--   x <- L.Var <$> getLlvmVar
--   load <- setVar $
--     L.Load L.I8 L.Ptr x
--
--   pure
--     [ getelementptr
--     , load
--     ]
--
--
-- termToLlvm (Case n def alts) = do
--     x <- getVar n
--     clauses <- altsToLlvm
--     defClause <- L.Label "clause_def" <$> termToLlvm def
--
--     pure $
--       [ L.Switch caseType x (L.MkVar "clause_def") alts'
--       ]
--       ++
--       snoc clauses defClause
--   where
--     alts'
--       | Just t <- isLitCase =
--         zipWith
--           (\(AltLit l _) -> L.Alt t $ litToLlvm l)
--           alts
--           clauseLabels
--       | isNodeCase =
--         zipWith
--           (\(AltNode t _) -> L.Alt L.I8 $ llvmLit $ tag t)
--           alts
--           clauseLabels
--       | otherwise = __IMPOSSIBLE__
--
--     caseType
--       | Just t <- isLitCase = t
--       | isNodeCase = L.I8
--       | otherwise  = __IMPOSSIBLE__
--
--     isLitCase :: Maybe L.Type
--     isLitCase =
--       mapM (fmap litToLlvmType . isLit) alts >>= \case
--         []   -> __IMPOSSIBLE__
--         x:xs -> foldr sameType (Just x) xs
--       where
--         sameType t1 (Just t2)
--           | t1 == t2  = Just t2
--           | otherwise = Nothing
--         sameType _ Nothing  = Nothing
--
--         isLit (AltLit lit _) = Just lit
--         isLit _              = Nothing
--
--     isNodeCase = flip all alts $ \case
--       AltNode{} -> True
--       _         -> False
--
--     clauseLabels = take (length alts) mkClauseLabels
--
--     altsToLlvm =
--         zipWithM (\s i -> L.Label s <$> altToLlvm i) clauseLabels alts
--
-- termToLlvm (Unit v) = do
--   v' <- valToLlvm v
--   pure [L.Ret L.Ptr v']
--
--
--
-- termToLlvm (App (Prim PSub64) vs) = do
--   pure [L.Comment "sub64"]
-- termToLlvm (App (Prim PAdd64) vs) = do
--   pure [L.Comment "add64."]
--
--
-- termToLlvm (App v vs) = do
--   v' <- valToLlvm v
--   -- FIXME
--   vs' <- zip (repeat L.Ptr) <$> mapM valToLlvm vs
--   singleton <$> setVar (L.Call L.Fastcc L.Ptr L.Global v' vs')
--
--
-- termToLlvm t = pure $ singleton $ L.Comment $ prettyShow t
--
-- -- termToLlvm t = error $ "not implemented: " ++ show t
--
--
-- valToLlvm :: Val -> M L.Val
-- valToLlvm (Lit lit)  = pure $ litToLlvm lit
-- valToLlvm (Var n)    = L.Var <$> getVar n
-- valToLlvm (Def q)    = pure $ L.Var $ L.MkVar $ prettyShow q
-- valToLlvm (Node _ _) = pure L.Null -- FIXME
-- valToLlvm v          = error $ "valToLlvm not implemented: " ++ show v
--
-- litToLlvm :: Literal -> L.Val
-- litToLlvm = L.Lit
--
-- nodeType :: L.Type
-- nodeType = L.Alias $ L.MkVar "Node"
--
-- setVar :: MonadState Env m => L.Instruction -> m L.Instruction
-- setVar i = do
--   modify $ \env -> env {varCount = 1 + env.varCount}
--   x <- gets $ L.MkVar . show . varCount
--   pure $ L.SetVar x i
--
-- mkClauseLabels :: [String]
-- mkClauseLabels = map (("clause_" ++) . show) [1 ..]
--
-- listType :: L.Type
-- listType = L.Structure [L.I8, L.Ptr, L.Ptr]
--
-- nodeType' :: Int -> L.Type
-- nodeType' n = L.Structure $ L.I8 : replicate n L.Ptr
--
-- -- TODO use local and increase vars
-- -- PROBLEM multiple variables for a term means that de bruijn indices are off
-- altToLlvm :: Alt -> M [L.Instruction]
-- altToLlvm = \case
--   -- TODO claues
--   AltNode tag t -> do
--     scrutVar <- getVar 0
--     let arity = tagr_arity tag
--
--     -- TODO do same for full arity
--     (newVars, is) <- fmap (second concat . unzip) $ forM [1 .. arity] $ \n -> do
--       getelementptr <- setVar $
--         L.Getelementptr (nodeType' arity)
--           [ (L.Ptr, L.Var scrutVar)
--           , (L.I32, llvmLit 0)
--           , (L.I32, llvmLit n)]
--       x <- L.Var <$> getLlvmVar
--       load <- setVar $ L.Load nodeType L.Ptr x
--       name <- gets $ show . varCount
--       pure (VarInfo {name=name}, [getelementptr, load])
--
--     rhs <- local (\cxt -> cxt { vars = newVars ++ cxt.vars }) $ termToLlvm t
--     pure $ is ++ rhs
--
--   AltLit _ t -> termToLlvm t
--
--
--   AltVar t -> do
--     -- Add the most recent LLVM variable to context. FIXME
--     n <- gets varCount
--     let newVar = VarInfo {name = show n}
--     local (\cxt -> cxt { vars = newVar : cxt.vars }) (termToLlvm t)
--
--   AltEmpty t -> termToLlvm t
--
--
-- typeToLlvm :: Type -> M L.Type
-- typeToLlvm t = asks $ flip go t where
--
--   go cxt (I.El _ t)
--     | Just prim <- mprim = prim
--     where
--       mprim = Map.lookup t cxt.builtins >>= \case
--         BuiltinNat -> Just L.I64
--         b          -> error $ "Not implemented: " ++ prettyShow b
--
--   go _ (I.El _ t) = case t of
--     I.Def q _-> L.Alias $ L.MkVar $ prettyShow q
--     _         -> L.Ptr -- FIXME
--
--
-- litToLlvmType :: Literal -> L.Type
-- litToLlvmType LitNat{} = L.I64
--
--
-- getLlvmVar :: MonadState Env m => m L.Var
-- getLlvmVar = gets $ L.MkVar . show . varCount
--
-- getVar :: MonadReader Cxt m => Int -> m L.Var
-- getVar n = do
--   vs <- asks vars
--   pure $ maybe __IMPOSSIBLE__ (L.MkVar . name) (vs !!! n)
--
-- llvmLit :: Integral a => a -> L.Val
-- llvmLit = L.Lit . LitNat . toInteger
--
-- returnType :: Type -> ([Type], Type)
-- returnType = go . ([], ) where
--   go (acc, I.El _ (I.Pi I.Dom{unDom} I.NoAbs{unAbs})) = go (snoc acc unDom , unAbs)
--   go p = p
--
--














































-----------------------------------------------------------------------
-- * Other
-----------------------------------------------------------------------

-- manual :: [GrinDefinition]
-- manual = [evalDef, downFromDef, sumDef, mainDef] where
--   callEval = App (Def "eval") . singleton
--   nilN = "[]"
--   consN = "_∷_"
--   sumN = "sum"
--   downFromN = "downFrom"
--   printfN = "printf"
--   natN = "nat"
--
--   evalDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=eval, gr_name="eval", gr_arity=1}
--   eval =
--     Bind
--       (Fetch $ Var 0)
--       (AltVar
--         (Case 0 unreachable
--           [ AltNode CTag {tTag=0, tCon=natN, tArity=1} (Unit $ Var 1)
--           , AltNode CTag {tTag=1, tCon=nilN, tArity=0} (Unit $ Var 0)
--           , AltNode CTag {tTag=2, tCon=consN, tArity=2} (Unit $ Var 2)
--           , AltNode FTag {tTag=3, tDef=downFromN, tArity=1}
--               (Bind
--                 (App (Def downFromN) [Var 0])
--                 (AltVar
--                   (Bind
--                     (Update (Var 2) (Var 0))
--                     (AltEmpty
--                       (Unit $ Var 0)
--                     )
--                   )
--                 )
--               )
--           , AltNode FTag {tTag=4, tDef=sumN, tArity=1}
--               (Bind
--                 (App (Def sumN) [Var 0])
--                 (AltVar
--                   (Bind
--                     (Update (Var 2) (Var 0))
--                     (AltEmpty
--                       (Unit $ Var 0)
--                     )
--                   )
--                 )
--               )
--           ]
--         )
--       )
--
--   downFromDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=downFrom, gr_name=downFromN, gr_arity=1}
--   downFrom =
--     Bind
--       (callEval $ Var 0)
--       (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--         (Case 0
--           (Bind
--             (App (Prim PSub64) [Var 0, Lit $ LitNat 1])
--             (AltVar
--               (Bind
--                 (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 0])
--                 (AltVar
--                   (Bind
--                     (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
--                     (AltVar
--                       (Bind
--                         (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 2])
--                         (AltVar
--                           (Unit $ Node CTag {tTag=2, tCon=consN, tArity=2} [Var 0, Var 1])
--                         )
--                       )
--                     )
--                   )
--                 )
--               )
--             )
--           )
--           [ AltLit (LitNat 0) $ Unit $ Node CTag {tTag=1, tCon=nilN, tArity=0} []]
--         )
--       )
--
--
--   sumDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=sum, gr_name=sumN, gr_arity=1}
--   sum =
--     Bind
--       (callEval $ Var 0)
--       (AltVar
--         (Case 0 unreachable
--           [ AltNode
--               CTag {tTag=1, tCon=nilN, tArity=0}
--               (Unit $ Node CTag { tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 0])
--           , AltNode
--               CTag {tTag=2, tCon=consN, tArity=2}
--               (Bind
--                 (callEval $ Var 1)
--                 (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--                   (Bind
--                     (App (Def sumN) [Var 1])
--                     (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--                       (Bind
--                         (App (Prim PAdd64) [Var 1, Var 0])
--                         (AltVar
--                           (Unit $ Node CTag {tTag=0, tCon=natN, tArity=1} [Var 0])
--                         )
--                       )
--                     )
--                   )
--                 )
--               )
--
--
--           ]
--         )
--       )
--
--
--   mainDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=main, gr_name="main", gr_arity=0}
--   main =
--     Bind
--       (Store $ Node CTag {tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 4])
--       (AltVar
--         (Bind
--           (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
--           (AltVar
--             (Bind
--               (Store $ Node FTag {tTag=4, tDef=sumN, tArity=1} [Var 0])
--               (AltVar
--                 (Bind
--                   (callEval $ Var 0)
--                   (AltNode CTag {tTag=0, tCon=natN, tArity=1}
--                     (App (Def printfN) [Var 0])
--                   )
--                 )
--               )
--             )
--           )
--         )
--       )
--

-- llvmCompileDef env menv isMainModule Defn{defName, defType, theDef=Function{}} = do
--     treeless <- maybeM __IMPOSSIBLE__ normalizeNames $ toTreeless LazyEvaluation defName
--     let (arity, treeless') = skipLambdas treeless
--
--     term' <- treelessToGrin isMain (simplifyApp treeless') arity
--     gr_args <- replicateM arity freshAbs
--     gr_return <- boolToMaybe (not isMain) <$> freshAbs
--
--     let gDef  =
--           GrinDefinition
--             { gr_type = Just defType
--             , gr_name = prettyShow defName
--             , gr_arity = arity
--             , gr_term = term'
--             , gTreeless = Just $ simplifyApp treeless
--             , gr_args = gr_args
--             , gr_return = gr_return
--             }
--     pure $ Just gDef
--   where
--     isMain = isNamedMain && isMainModule == IsMain
--     isNamedMain = "main" == (prettyShow . nameConcrete . qnameName) defName
--
-- llvmCompileDef env menv isMain def@Defn{defName, theDef=Primitive{primName}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Primitive:", pretty defName, pretty primName]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def@Defn{defName, theDef=Datatype{dataCons}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Datatype: ", pretty defName, pretty dataCons]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def@Defn{defName, defType, theDef=Constructor{conArity, conData}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Constructor:", pretty defName, pretty conData, pretty conArity]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def = pure Nothing
--


trace' :: String -> a -> a
trace' s = unsafePerformIO . (appendFile "trace.log" (s ++ "\n") $>)
