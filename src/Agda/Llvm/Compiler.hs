{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ViewPatterns             #-}

module Agda.Llvm.Compiler (module Agda.Llvm.Compiler) where

import           Control.DeepSeq                (NFData)
import           Control.Monad                  (forM, join, mapAndUnzipM,
                                                 replicateM, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State            (MonadState, State, StateT,
                                                 evalState, evalStateT, gets,
                                                 modify)
import           Data.Foldable                  (foldrM, toList)
import           Data.Function                  (on)
import           Data.List                      (intercalate, mapAccumL,
                                                 singleton, unzip4)
import           Data.List.NonEmpty.Extra       ((|:), (|>))
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
import           Agda.Llvm.Llvm                 (Instruction (SetVar))
import qualified Agda.Llvm.Llvm                 as L
import           Agda.Llvm.TreelessTransform
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal            (Literal (LitNat))
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1               (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1               as List1
import           Agda.Utils.Maybe
import           Control.Monad.Reader           (MonadReader, Reader,
                                                 ReaderT (runReaderT), asks,
                                                 runReader)



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
    -- , Option []  ["emit-llvm"] (Arg enable) TODO
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

  let llvm_ir = grinToLlvm defs_introduceRegisters
      llvm_ir_str =
        unlines
          [ "target triple = \"x86_64-pc-linux-gnu\""
          , "target datalayout = \"p:64:64:64\""
          , "declare void @printf(ptr, ...)"
          , "declare ptr @malloc(i64)"
          , "%Node = type [4 x i64]"
          , "@\"%d\" = private constant [4 x i8] c\"%d\\0A\\00\", align 1"
          ] ++ intercalate "\n\n" (map prettyShow llvm_ir)
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * LLVM IR"
    putStrLn "------------------------------------------------------------------------\n"
    -- putStrLn llvm_ir_str
    writeFile "program.ll" llvm_ir_str

-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • Need to deal with erased arguments and parameters
--
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
        f TErased    (ss, vs) = pure (ss, vs)
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
          f TErased    (ss, vs) = pure (ss, vs)
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
        f TErased    (ss, vs) = pure (ss, vs)
        f _          _        = __IMPOSSIBLE__

    (stores, vs) <-  foldrM f ([], []) as

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
-- * LLVM code generation
-----------------------------------------------------------------------

grinToLlvm :: [GrinDefinition] -> [L.Instruction]
grinToLlvm defs =
  flip evalState initCodeGenEnv $ forM defs $ \def ->
  let cxt = initCodeGenCxt defs def in
  (lensFreshVar .= 1) *> runReaderT (runCodeGen $ definitionToLlvm def) cxt

-- evalCodeGen :: GlobalsTy -> CodeGen a -> a
-- evalCodeGen globalsTy x =
--   evalState (runReaderT (runCodeGen x) $ initCodeGenCxt globalsTy) initCodeGenEnv
--

mkGlobalTys :: [GrinDefinition] -> Map L.GlobalId ([L.Type], L.Type)
mkGlobalTys defs =
  Map.fromList $ for defs $ \def ->
    let types
          | getShortName def == "main" = ([], L.Void)
          | otherwise = (replicate def.gr_arity L.I64, L.nodeTySyn) in
    (L.mkGlobalId def.gr_name, types)

definitionToLlvm :: GrinDefinition -> CodeGen L.Instruction
definitionToLlvm def = do
  let f = L.mkGlobalId def.gr_name
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  let args = zip argsTy $ map L.mkLocalId def.gr_args
  L.Define L.Fastcc returnTy f args <$> termToLlvm def.gr_term

termToLlvm :: Term -> CodeGen (List1 L.Instruction)
termToLlvm (Case (Var n) t alts `Bind` alt) = do
    (alts', i_blocks, xs_label, xs_res) <- fmap unzip4 $ forM alts $
      \case
        CAltTag tag t        -> (`go` t) =<< tagNumLookup tag
        CAltLit (LitNat n) t -> go (fromInteger n) t
        _                    -> __IMPOSSIBLE__
    x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
    let i_switch = L.switch x "%alt_default" alts'
    let x_default_res = "%alt_default_res"
    i_default <- mkBlock "alt_default" x_default_res t
    let i_phi = L.phi L.nodeTySyn $ zip xs_res xs_label |: (x_default_res, "%alt_default")
    is <- laltToContinuation alt i_phi
    pure $ (i_switch :| i_blocks) |> i_default |> L.Label "continue" is
  where
    go n t = do
      let s = "alt_" ++ show n
          x_label = L.mkLocalId s
          x_res = L.mkLocalId (s ++ "_res")
          alt = L.alt (L.mkLit n) x_label
      block <- mkBlock s x_res t
      pure (alt, block, x_label, x_res)

    mkBlock s x =
      let cont i = pure $ SetVar x i <|  L.Br "%continue" :| [] in
      fmap (L.Label s) . continuationLocal cont . termToLlvm

termToLlvm (Store _ v `Bind` LAltVar (L.mkLocalId -> x) t) = do
  (is1, v') <- valToLlvm v
  is2 <- varLocal x (termToLlvm t)
  pure $ L.SetVar x (L.malloc (L.size L.nodeTy)) <| L.store L.I64 v' x <| (is1 `List1.prependList` is2)

termToLlvm (FetchOffset n offset `Bind` alt) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (x_unnamed_ptr, i_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed, i_getelemtptr) <- setVar (L.getelementptr x_unnamed_ptr offset)
  List1.prependList [i_inttoptr, i_getelemtptr] <$> laltToContinuation alt (L.load L.I64 x_unnamed)

termToLlvm (App (Def (L.mkGlobalId -> f)) vs `Bind` alt) = do
  (concat -> is, vs') <- mapAndUnzipM valToLlvm vs
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  let i_call = L.Call L.Fastcc returnTy f $ zip argsTy vs'
  List1.prependList is <$> laltToContinuation alt i_call

termToLlvm (App (Prim prim) [v1, v2] `Bind` alt) = do
  let op = case prim of
        PAdd -> L.add64
        PSub -> L.sub64
        _    -> __IMPOSSIBLE__
  (is1, v1') <- valToLlvm v1
  (is2, v2') <- valToLlvm v2
  List1.prependList (is1 ++ is2) <$> laltToContinuation alt (op v1' v2')

-- FIXME
termToLlvm (Unit v `Bind` alt) = do
  (is, v') <- valToLlvm v
  List1.prependList is <$> go alt v'
  where
    go (LAltVar (L.mkLocalId -> x) t) v = do
      is <- varLocal x (termToLlvm t)
      (x_unnamed_ptr, i_alloca) <- alloca (L.typeOf v)
      pure $ i_alloca <| L.store L.I64 v x_unnamed_ptr  <| SetVar x (L.ptrtoint x_unnamed_ptr) <| is
    go (LAltConstantNode _ (map L.mkLocalId -> xs) t) v = do
      let is_extractvalue = zipWith (\x -> SetVar x . L.extractvalue v) xs [1 ..]
      List1.prependList is_extractvalue <$> varLocals xs (termToLlvm t)
    go (LAltVariableNode (L.mkLocalId -> x) (map L.mkLocalId -> xs) t) v = do
      let is_extractvalue = zipWith (\x -> SetVar x . L.extractvalue v) (x : xs) [0 ..]
      List1.prependList is_extractvalue <$> varLocals (x : xs) (termToLlvm t)
    go (LAltEmpty t) _ = termToLlvm t

termToLlvm (UpdateTag _ n v `BindEmpty` t) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (is1, v') <- valToLlvm v
  is2 <- termToLlvm t
  pure $ (is1 |: L.store L.I64 v' x) <> is2


termToLlvm (Case (Var n) t alts) = do
  (alts', i_blocks) <- fmap unzip $ forM alts $
    \case
        CAltTag tag t        -> (`go` t) =<< tagNumLookup tag
        CAltLit (LitNat n) t -> go (fromInteger n) t
        _                    -> __IMPOSSIBLE__

  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  let i_switch = L.switch x "%alt_default" alts'
  i_default <- mkBlock "alt_default" t
  pure $ (i_switch :| i_blocks) |> i_default
  where
    go n t = do
      let s = "alt_" ++ show n
          alt = L.alt (L.mkLit n) (L.mkLocalId s)
      i_block <- mkBlock s t
      pure (alt, i_block)

    mkBlock s =
      let cont i = do
            (L.LocalId -> x_unnamed, i_setVar) <- setVar i
            pure $ i_setVar <| L.RetNode x_unnamed :| [] in
      fmap (L.Label s) . continuationLocal cont . termToLlvm

termToLlvm (Unit (VariableNode n vs)) = do
  v <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  (concat -> is1, (v <|) -> vs') <- List1.unzip <$> mapM valToLlvm vs
  let (vs'', v') = List1.initLast vs'
  (v'', is2) <- forAccumM L.Undef (zip vs'' [0 ..]) $ \acc (v, offset) -> do
    (L.LocalId -> x_unnamed, i_insertvalue) <- setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  cont <- view continuationLens
  let offset = length vs
  is3 <- cont (L.insertvalue v'' v' offset)
  pure $ (is1 ++ is2) `List1.prependList` is3

termToLlvm (Unit (Tag tag)) = do
  (L.mkLit -> v) <- tagNumLookup tag
  cont <- view continuationLens
  cont (L.insertvalue L.Undef v 0)

-- TODO: perhaps remove printf from GRIN and instead use a printf continuation
termToLlvm (App (Def "printf") [v]) = do
  (is, v') <- valToLlvm v
  let format = L.GlobalId $ L.mkGlobalId @String "%d"
      i_call = L.Call L.Fastcc L.Void "@printf" [(L.Ptr, format), (L.I64, v')]
  pure $ is `List1.prependList` (i_call <| L.RetVoid :| [])

termToLlvm (Error TUnreachable) = pure $ List1.singleton L.Unreachable
termToLlvm t = error $ "BAD: " ++ show t

laltToContinuation :: LAlt -> L.Instruction -> CodeGen (List1 L.Instruction)
laltToContinuation (LAltVar (L.mkLocalId -> x) t) i = (SetVar x i <|) <$> varLocal x (termToLlvm t)
laltToContinuation (LAltConstantNode _ (map L.mkLocalId -> xs) t) i = do
  (L.LocalId -> x_unnamed, i_setVar) <- setVar i
  let is_extractvalue = zipWith (\x -> SetVar x . L.extractvalue x_unnamed) xs [1 ..]
  is <- varLocals xs (termToLlvm t)
  pure $ (i_setVar :| is_extractvalue) <> is
laltToContinuation (LAltVariableNode (L.mkLocalId -> x) (map L.mkLocalId -> xs) t) i = do
  (L.LocalId -> x_unnamed, i_setVar) <- setVar i
  let is_extractvalue = zipWith (\x -> SetVar x . L.extractvalue x_unnamed) (x : xs) [0 ..]
  is <- varLocals (x : xs) (termToLlvm t)
  pure $ (i_setVar :| is_extractvalue) <> is
laltToContinuation (LAltEmpty t) _ = termToLlvm t

valToLlvm :: Val -> CodeGen ([L.Instruction], L.Val)
valToLlvm (Var n) = maybe __IMPOSSIBLE__ (([],) . L.LocalId) <$> varLookup n
valToLlvm (Def s) = pure ([], L.LocalId $ L.mkLocalId s)
valToLlvm (Prim _) = __IMPOSSIBLE__
valToLlvm (Lit lit) = pure ([], L.Lit lit)
valToLlvm (VariableNode n vs) = do
  v <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  (concat -> is1, (v <|) -> vs') <- List1.unzip <$> mapM valToLlvm vs
  (v', is2) <- forAccumM L.Undef (List1.zip vs' (0 :| [1 ..])) $ \acc (v, offset) -> do
    (L.LocalId -> x_unnamed, i_insertvalue) <- setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  pure (is1  ++ toList is2, v')
valToLlvm ConstantNode{} = __IMPOSSIBLE__
valToLlvm (Tag tag) = ([], ) . L.Lit <$> tagToLlvm tag
valToLlvm Empty = __IMPOSSIBLE__

data CodeGenEnv = CodeGenEnv
  { cg_freshVarNum :: Int
  , cg_freshTagNum :: Int
  , cg_tagNums     :: Map Tag Int
  }

lensFreshVar :: Lens' CodeGenEnv Int
lensFreshVar f cxt = f cxt.cg_freshVarNum <&> \n -> cxt{cg_freshVarNum = n}

varFresh :: MonadState CodeGenEnv m => m Int
varFresh = use lensFreshVar <* modify (over lensFreshVar succ)

lensFreshTagNum :: Lens' CodeGenEnv Int
lensFreshTagNum f env = f env.cg_freshTagNum <&> \n -> env{cg_freshTagNum = n}

lensTagNums :: Lens' CodeGenEnv (Map Tag Int)
lensTagNums f env = f env.cg_tagNums <&> \n -> env{cg_tagNums = n}

tagNumFresh :: MonadState CodeGenEnv m => Tag -> m Int
tagNumFresh tag = do
  whenJustM (Map.lookup tag <$> use lensTagNums) __IMPOSSIBLE__
  n <- use lensFreshTagNum
  modify $ over lensFreshTagNum succ
  modify $ over lensTagNums $ Map.insert tag n
  pure n

tagNumLookup :: MonadState CodeGenEnv m => Tag -> m Int
tagNumLookup tag =
  fromMaybeM (tagNumFresh tag) $ gets $ Map.lookup tag . (^. lensTagNums)

initCodeGenEnv :: CodeGenEnv
initCodeGenEnv = CodeGenEnv
  { cg_freshVarNum = 0
  , cg_freshTagNum = 0
  , cg_tagNums     = mempty
  }

data CodeGenCxt = CodeGenCxt
  { cg_vars         :: [L.LocalId]
  , cg_globalsTy    :: GlobalsTy
  , cg_continuation :: Continuation
  }

type Continuation = L.Instruction -> CodeGen (List1 L.Instruction)

type GlobalsTy = Map L.GlobalId ([L.Type], L.Type)

initCodeGenCxt :: [GrinDefinition] -> GrinDefinition -> CodeGenCxt
initCodeGenCxt defs def =
    CodeGenCxt
      { cg_vars = vars
      , cg_globalsTy = globalsTy
      , cg_continuation = cont
      }
  where
    vars = map L.mkLocalId def.gr_args
    globalsTy = mkGlobalTys defs
    cont
      | getShortName def == "main" = const $ pure $ List1.singleton L.RetVoid
      | otherwise = \i -> do
        (L.LocalId -> x_unnamed, i_setVar) <- setVar i
        pure $ i_setVar <| L.RetNode x_unnamed :| []

lensVars :: Lens' CodeGenCxt [L.LocalId]
lensVars f cxt = f cxt.cg_vars <&> \xs -> cxt{cg_vars = xs}

varLocals :: MonadReader CodeGenCxt m => [L.LocalId] -> m a -> m a
varLocals = foldl (.: locally lensVars . (:)) id

varLocal :: MonadReader CodeGenCxt m => L.LocalId -> m a -> m a
varLocal x = locally lensVars (x :)

varLookup :: MonadReader CodeGenCxt m => Int -> m (Maybe L.LocalId)
varLookup n = asks $ varLookup' n

varLookup' :: Int -> CodeGenCxt -> Maybe L.LocalId
varLookup' n cxt = (cxt ^. lensVars) !!! n

globalsTyLens :: Lens' CodeGenCxt GlobalsTy
globalsTyLens f cxt = f cxt.cg_globalsTy <&> \ts -> cxt{cg_globalsTy = ts}

globalLookup :: MonadReader CodeGenCxt m => L.GlobalId -> m (Maybe ([L.Type], L.Type))
globalLookup f = Map.lookup f <$> view globalsTyLens

continuationLens :: Lens' CodeGenCxt Continuation
continuationLens f cxt = f cxt.cg_continuation <&> \cont -> cxt{cg_continuation = cont}

continuationLocal :: MonadReader CodeGenCxt m => Continuation -> m a -> m a
continuationLocal cont = locally continuationLens (const cont)

newtype CodeGen a = CodeGen{runCodeGen :: ReaderT CodeGenCxt (State CodeGenEnv) a}
  deriving (Functor, Applicative, Monad, MonadState CodeGenEnv, MonadReader CodeGenCxt)

tagToLlvm :: Tag -> CodeGen Literal
tagToLlvm = fmap (LitNat . toInteger) . tagNumLookup

todoLlvm s = pure $ L.Comment ("TODO: " ++ take 30 (prettyShow s)) :| []

setVar :: L.Instruction -> CodeGen (L.LocalId, L.Instruction)
setVar i = varFresh <&> \(L.mkUnnamed -> x) -> (x, SetVar x i)

alloca :: L.Type -> CodeGen (L.LocalId, L.Instruction)
alloca t = setVar $ L.Alloca t

storeOffset :: Int -> L.Type -> L.Val -> L.LocalId -> CodeGen (List1 Instruction)
storeOffset offset t v x = do
  (x_unnamed_ptr, i_getelemtptr) <- setVar (L.getelementptr x offset)
  pure $ i_getelemtptr <| L.store t v x_unnamed_ptr :| []
