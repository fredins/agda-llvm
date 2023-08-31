{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ViewPatterns             #-}

module Agda.Llvm.Compiler (module Agda.Llvm.Compiler) where

import           Control.DeepSeq                (NFData)
import           Control.Monad                  (forM, mapAndUnzipM, replicateM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (MonadReader,
                                                 ReaderT (runReaderT), asks)
import           Control.Monad.State            (MonadState, State, StateT,
                                                 evalStateT, gets, modify,
                                                 runState)
import           Data.Bifunctor                 (Bifunctor (bimap, first))
import           Data.Foldable                  (foldrM, toList)
import           Data.Function                  (on)
import           Data.List                      (intercalate, mapAccumL,
                                                 singleton, sortOn, unzip4)
import           Data.List.NonEmpty.Extra       ((|:), (|>))
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Tuple.Extra               (swap)
import           GHC.Generics                   (Generic)
import           Prelude                        hiding ((!!))

import           Agda.Compiler.Backend          hiding (Prim, getPrimitive,
                                                 initEnv)
import           Agda.Interaction.Options
import           Agda.Llvm.Grin
import           Agda.Llvm.GrinInterpreter      (interpretGrin)
import           Agda.Llvm.GrinTransformations
import           Agda.Llvm.HeapPointsTo
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
      "Compile program using the LLVM backend"
    , Option []  ["emit-llvm"] (NoArg enable)
      "Emit LLVM IR"
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

  -- res_inlineEval <- interpretGrin defs_inlineEval
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_inlineEval

  let defs_normalize = map (updateGrTerm normalise) defs_inlineEval
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalize

  -- res_normalise <- interpretGrin defs_normalize
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_normalise

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_normalize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  -- res_leftUnitLaw <- interpretGrin defs_leftUnitLaw
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_leftUnitLaw

  defs_specializeUpdate <- mapM (specializeUpdate absCxt) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Specialize Update"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_specializeUpdate

  -- res_specializeUpdate <- interpretGrin defs_specializeUpdate
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_specializeUpdate

  let defs_normalize = map (updateGrTerm normalise) defs_specializeUpdate
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalize

  -- res_normalise <- interpretGrin defs_normalize
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_normalise

  defs_vectorize <- mapM (lensGrTerm $ vectorize tagInfo_inlineEval) defs_normalize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Vectorization"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_vectorize

  -- res_vectorize <- interpretGrin defs_vectorize
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_vectorize

  let defs_simplifyCase = map (updateGrTerm simplifyCase) defs_vectorize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Simplify case"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_simplifyCase

  -- res_simplifyCase <- interpretGrin defs_simplifyCase
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_simplifyCase

  let defs_splitFetch = map (updateGrTerm splitFetch) defs_simplifyCase
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Split fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_splitFetch

  -- res_splitFetch <- interpretGrin defs_splitFetch
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_splitFetch

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_splitFetch
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  -- res_leftUnitLaw <- interpretGrin defs_leftUnitLaw
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_leftUnitLaw

  defs_rightHoistFetch <- mapM (lensGrTerm rightHoistFetch) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Right hoist fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_rightHoistFetch

  -- res_rightHoistFetch <- interpretGrin defs_rightHoistFetch
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_rightHoistFetch

  -- Not used
  --
  -- defs_introduceRegisters  <- mapM (lensGrTerm introduceRegisters) defs_rightHoistFetch
  -- liftIO $ do
  --   putStrLn "\n------------------------------------------------------------------------"
  --   putStrLn "-- * Introduce Registers"
  --   putStrLn "------------------------------------------------------------------------\n"
  --   putStrLn $ intercalate "\n\n" $ map prettyShow defs_introduceRegisters
  --
  -- res_introduceRegisters <- interpretGrin defs_introduceRegisters
  -- liftIO $ putStrLn $ "\nResult: " ++ show res_introduceRegisters

  let (llvm_ir, tagsToInt) = grinToLlvm defs_rightHoistFetch
      header =
        unlines
          [ "target triple = \"x86_64-unknown-linux-gnu\""
          , "declare void @printf(ptr, ...)"
          , "declare ptr @malloc(i64)"
          , "%Node = type [3 x i64]" ]
         ++ "@\"%d\" = private constant [4 x i8] c\"%d\\0A\\00\", align 1"

         -- -- debug
         -- ++ unlines
         -- [ "\n@\"sum\" = private constant [9 x i8] c\"sum: %d\\0A\\00\", align  1"
         -- , "@\"downFrom\" = private constant [14 x i8] c\"downFrom: %d\\0A\\00\", align 1"
         -- , "@\"_-_\" = private constant [9 x i8] c\"_-_: %d\\0A\\00\", align 1"
         -- , "@\"_+_\" = private constant [9 x i8] c\"_+_: %d\\0A\\00\", align 1"
         -- ]

      tags_table = "; Tag numbering table:\n" ++ prettyShow (align 20 (map (bimap ((++) "; " . show) pretty . swap) (sortOn snd (Map.toList tagsToInt))))
      defs = intercalate "\n\n" (map prettyShow llvm_ir)
      program = intercalate "\n\n" [header, tags_table, defs]

  liftIO (writeFile "program.ll" program)

-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • Fix super ugly code
-- • Need to deal with erased arguments and parameters
-- • Refactor (use Reader instead of State)
-- • Use de Bruijn substitions
-- • Use bind combinators
-- • Reuse evaluated variables (WIP)
-- • Fill in rest of the patterns

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors

treelessToGrin :: [TreelessDefinition] -> TCM [GrinDefinition]
treelessToGrin defs =
  forM defs $ \def -> do
  gr_term <- evalStateT (rScheme def.tl_term) (initGEnv def primitives)
  gr_args <- replicateM def.tl_arity freshAbs
  gr_return <- boolToMaybe (not def.tl_isMain) <$> freshAbs
  pure $ GrinDefinition
    { gr_name = def.tl_name
    , gr_primitive = def.tl_primitive
    , gr_isMain = def.tl_isMain
    , gr_arity = def.tl_arity
    , gr_type = Just def.tl_type
    , gr_term = gr_term
    , gr_args = gr_args
    , gr_return = gr_return
    }
  where
  primitives = mapMaybe (\def -> (, def.tl_name) <$> def.tl_primitive) defs

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
    primitives <- gets primitives
    currentDef <- gets currentDef
    alt <- laltConstantNode natTag (printf 0)
    let res t
          | isMain = Bind t alt
          | otherwise = t
    case t of
      TPrim prim
        | Just prim' <- currentDef.tl_primitive
        , prim' == prim -> value <$> appPrim res prim as
        | otherwise -> value <$> appDef res (fromMaybe __IMPOSSIBLE__ (lookup prim primitives)) as
      TDef q     -> value <$> appDef res (prettyShow q) as
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
    appDef res name as = do
      let
        nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

        f (TLit lit) (ss, vs) = do
            s <- store $ ConstantNode natTag $ Lit lit :| []
            pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f TErased    (ss, vs) = pure (ss, vs)
        f _          _        = __IMPOSSIBLE__

      (stores, vs) <- foldrM f ([], []) as
      let fin = res $ App (Def name) vs
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

-- TODO need to keep track of evaluated variables
-- 𝓡 [x] = eval 0 ; λ x → unit 0
rScheme (TVar n) = eval n `bindVar` Unit (Var 0)

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
        f t          _        = error $ "CSCHEMEAPP: " ++ show t

    (stores, vs) <-  foldrM f ([], []) as

    primitives <- gets primitives
    let tag
          | TDef q <- t = FTag{tDef = prettyShow q, tArity = length as}
          | TPrim prim <- t =  FTag {tDef=fromMaybe __IMPOSSIBLE__ (lookup prim primitives), tArity=length as}
          | otherwise = __IMPOSSIBLE__

    t <- store $ ConstantNode tag $ caseList vs __IMPOSSIBLE__ (:|)
    abs <- freshAbs
    let fin = Bind t . LAltVar abs
    stores' <- foldrM (\t ts -> (\abs -> Bind t . LAltVar abs . ts) <$> freshAbs) fin stores
    pure $ mkWithOffset nLits stores'

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
  { gVars      :: [GVarInfo]
  , isMain     :: Bool
  , primitives :: [(TPrim, String)]
  , currentDef :: TreelessDefinition
  }

initGEnv :: TreelessDefinition -> [(TPrim, String)] -> GEnv
initGEnv def primitives = GEnv
  { gVars = replicate def.tl_arity mkVar
  , isMain = def.tl_isMain
  , primitives = primitives
  , currentDef = def
  }

type G = StateT GEnv TCM

data WithOffset a = WithOffset
  { offset :: Int
  , value  :: a
  }

mkWithOffset :: Int -> a -> WithOffset a
mkWithOffset n a = WithOffset{offset = n, value = a}
-- rScheme (TApp t as) = do
--     isMain <- gets getIsMain
--     alt <- laltConstantNode natTag (printf 0)
--     primitives <- gets primitives
--     mprim <- gets getPrimitive
--     let res t
--           | isMain = Bind t alt
--           | otherwise = t
--     case t of
--       TPrim prim
--         | Just prim' <- mprim
--         , prim' == prim -> appPrim prim as
--         | otherwise ->
--           let name = fromMaybe __IMPOSSIBLE__ (lookup prim primitives) in
--           snd <$> appDef res name as
--       TDef q     -> snd <$> appDef res (prettyShow q) as
--       TCon q     -> snd <$> appCon res (prettyShow q) as
--       _          -> __IMPOSSIBLE__
--   where
    -- appPrim prim as = do
    --     offsets <- asks evaluatedOffsets
    --     x <- freshAbs
    --     let mkT vs =
    --           App (Prim prim) vs `Bind` LAltVar x
    --           (Unit (ConstantNode natTag (List1.singleton (Var 0))))
    --     evaluateValues offsets as mkT
    --
    --   where
    --     -- TODO need to add local abss?
    --     evaluateValues :: MonadFresh Int mf => [Int -> Int] -> [TTerm] -> ([Val] -> Term) -> mf Term
    --     evaluateValues offsets vs mkT = (f . raise n . mkT) vs' where
    --       ((f, n), vs') =
    --         forAccumR (pure, 0) vs $ \(f, m) v ->
    --           caseEither (mkEval offsets v)
    --             ((f, m),)
    --             (\f' -> ((f' <=< f, succ m), Var (m - n)))
    --
    --     mkEval :: MonadFresh Int mf => [Int -> Int] -> TTerm -> Either Val (Term -> mf Term)
    --     mkEval offsets (TVar n) =
    --       let n' = applyEvaluatedOffset' n offsets in
    --       if n' == n then Right (bindVar (eval n)) else Left (Var n')
    --     mkEval _ (TLit lit) = Left (Lit lit)
    --     mkEval _ _ = __IMPOSSIBLE__
    --
    -- appDef res name as = do
    --   (stores, vs) <- foldrM f ([], []) as
    --   let fin = res $ App (Def name) vs
    --   stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores
    --   pure (nLits, stores')
    --   where
    --     nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as
    --
    --     f (TLit lit) (ss, vs) = do
    --         s <- store $ ConstantNode natTag $ Lit lit :| []
    --         pure (s : ss, Var (length ss) : vs)
    --     f (TVar n)   (ss, vs) = do
    --       n' <- applyEvaluatedOffset n
    --       pure (ss, Var (n' + nLits) : vs)
    --     f TErased (ss, vs) = pure (ss, vs)
    --     f _ _ = __IMPOSSIBLE__
    --
    -- appCon res q as = do
    --     let
    --       nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as
    --
    --       f (TLit lit) (ss, vs) = do
    --         s <- store $ ConstantNode natTag $ Lit lit :| []
    --         pure (s : ss, Var (length ss) : vs)
    --       f (TVar n)   (ss, vs) = do
    --         n' <- applyEvaluatedOffset n
    --         pure (ss, Var (n' + nLits) : vs)
    --       f TErased    (ss, vs) = pure (ss, vs)
    --       f _          _        = __IMPOSSIBLE__
    --
    --     (stores, vs) <- foldrM f ([], []) as
    --     let fin = res $ Unit $ ConstantNode tag $ caseList vs __IMPOSSIBLE__ (:|)
    --     stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores
    --
    --     pure (length stores, stores')
    --   where
    --     tag = CTag{tCon = prettyShow q, tArity = length as}

-- -- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
-- rScheme (TLet t1 t2)
--   | TApp t as <- t1 = do
--     (offset, t1') <- cSchemeApp t as
--     t2' <- rScheme $ raiseFrom 1 offset t2
--     pure $ t1' t2'
--
--   | TLet t1 t2 <- t1 = do
--     t1' <- cScheme t1
--     t2' <- rScheme t2
--     Bind t1' <$> laltVar t2'
--
-- rScheme t = error $ "TODO rScheme " ++ show t


-- cScheme :: TTerm -> GrinGen Term
-- cScheme t = error $ "TODO cScheme " ++ show t

-- | 𝓒 [foo x y] = store (Ffoo @1 @0)
--
--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
-- cSchemeApp :: TTerm -> Args -> GrinGen (Int, Term -> Term)
-- cSchemeApp (TDef q) as = cSchemeApp' (prettyShow q) as
-- cSchemeApp (TPrim prim) as = do
--   isSelf <- gets ((Just prim ==) . getPrimitive)
--   if isSelf then do
--     let vs = for as $ \case
--           TVar n -> Var n
--           _      -> __IMPOSSIBLE__
--     x <- freshAbs
--     pure (0, \t -> App (Prim prim) vs `Bind` LAltVar x t)
--   else do
--     name <- gets (fromMaybe __IMPOSSIBLE__ . lookup prim . primitives)
--     cSchemeApp' name as
-- cSchemeApp _ _ = __IMPOSSIBLE__

-- cSchemeApp' :: String -> Args -> GrinGen (Int, Term -> Term)
-- cSchemeApp' name as = do
--     (stores, vs) <-  foldrM f ([], []) as
--     t <- store $ ConstantNode tag $ caseList vs __IMPOSSIBLE__ (:|)
--     x <- freshAbs
--     let fin = Bind t . LAltVar x
--     stores' <- foldrM (\t ts -> (\abs -> Bind t . LAltVar abs . ts) <$> freshAbs) fin stores
--     pure (nLits, stores')
--   where
--     tag = FTag{tDef = name, tArity = length as}
--     nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as
--     f (TLit lit) (ss, vs) = do
--       s <- store $ ConstantNode natTag $ Lit lit :| []
--       pure (s : ss, Var (length ss) : vs)
--     f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
--     f TErased    (ss, vs) = pure (ss, vs)
--     f _          _        = __IMPOSSIBLE__


-----------------------------------------------------------------------
-- * LLVM code generation
-----------------------------------------------------------------------

grinToLlvm :: [GrinDefinition] -> ([L.Instruction], Map Tag Int)
grinToLlvm defs = (instructions, final_state.cg_tagNums)
  where
  (instructions, final_state) =
    flip runState initLlvmGenEnv $ forM defs $ \def ->
    let cxt = initLlvmGenCxt defs def in
    (freshUnnamedLens .= 1) *> runReaderT (runLlvmGen $ definitionToLlvm def) cxt

-- evalLlvmGen :: GlobalsTy -> LlvmGen a -> a
-- evalLlvmGen globalsTy x =
--   evalState (runReaderT (runLlvmGen x) $ initLlvmGenCxt globalsTy) initLlvmGenEnv
--

mkGlobalTys :: [GrinDefinition] -> Map L.GlobalId ([L.Type], L.Type)
mkGlobalTys defs =
  Map.fromList $ for defs $ \def ->
    let types
          | getShortName def == "main" = ([], L.Void)
          | otherwise = (replicate def.gr_arity L.I64, L.nodeTySyn) in
    (L.mkGlobalId def.gr_name, types)

definitionToLlvm :: GrinDefinition -> LlvmGen L.Instruction
definitionToLlvm def = do
  let f = L.mkGlobalId def.gr_name
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  let args = zip argsTy $ map L.mkLocalId def.gr_args
  L.Define L.Fastcc returnTy f args <$> termToLlvm def.gr_term

termToLlvm :: Term -> LlvmGen (List1 L.Instruction)
-- FIXME ugly
termToLlvm (Case (Var n) t alts `Bind` alt) = do
    alt_num <- freshAltNum
    let def = "default_" ++ show alt_num
    let x_def = L.mkLocalId def
    let x_def_res = L.mkLocalId (def ++ "_res")
    let continue = "continue_" ++ show alt_num

    let
      go n ((++ '_' : show alt_num) -> s) t = do
        let label = L.surroundWithQuotes s
            x_label = L.mkLocalId label
            alt = L.alt (L.mkLit n) x_label
            x_res = L.mkLocalId (L.surroundWithQuotes (s ++ "_res"))

        block <- mkBlock label x_res t
        pure (alt, block, x_label, x_res)

      mkBlock s x t =
        let cont i = pure $ L.SetVar x i <|  L.Br (L.mkLocalId continue) :| [] in
        L.Label s <$> continuationLocal cont (termToLlvm t)


    (alts', instruction_blocks, xs_label, xs_res) <- fmap unzip4 $ forM alts $
      \case
        CAltTag tag t        -> tagNumLookup tag >>= \n -> go n (prettyShow tag) t
        CAltLit (LitNat n) t -> go (fromInteger n) (prettyShow n) t
        _                    -> __IMPOSSIBLE__


    x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
    let instruction_switch = L.switch x x_def alts'
    instruction_default <- mkBlock def x_def t
    let phi_alts
          | isUnreachable t = caseList (zip xs_res xs_label) __IMPOSSIBLE__ (:|)
          | otherwise  = zip xs_res xs_label |: (x_def_res, x_def)
    let instruction_phi = L.phi L.nodeTySyn phi_alts
    instructions <- laltToContinuation alt instruction_phi

    pure $ (instruction_switch :| instruction_blocks) |> instruction_default |> L.Label continue instructions

-- FIXME ugly
termToLlvm (Case (Var n) t alts) = do
  alt_num <- freshAltNum
  let def = "default_" ++ show alt_num
  let x_def = L.mkLocalId def
  let
    go n s t = do
      let s' = L.surroundWithQuotes (s ++ '_' : show alt_num)
      let  alt = L.alt (L.mkLit n) (L.mkLocalId s')
      i_block <- mkBlock s' t
      pure (alt, i_block)

    mkBlock s t = L.Label s <$> termToLlvm t
  (alts', i_blocks) <- fmap unzip $ forM alts $
    \case
        CAltTag tag t        -> tagNumLookup tag >>= \n -> go n (prettyShow tag) t
        CAltLit (LitNat n) t -> go (fromInteger n) (prettyShow n) t
        _                    -> __IMPOSSIBLE__

  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  let i_switch = L.switch x x_def alts'
  i_default <- mkBlock def t
  pure $ (i_switch :| i_blocks) |> i_default


termToLlvm (Store _ v `Bind` LAltVar (L.mkLocalId -> x) t) = do
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instruction_malloc) <- setVar (L.malloc L.nodeSize)  -- L.alloca
  let instruction_store = L.store L.nodeTySyn v' x_unnamed_ptr
      instruction_ptrtoint = L.SetVar x (L.ptrtoint x_unnamed_ptr)
  instructions2 <- varLocal x (termToLlvm t)
  pure $ instructions1 `List1.prependList`
    (instruction_malloc <| instruction_store <| instruction_ptrtoint <| instructions2)

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


termToLlvm (UpdateTag _ n v `BindEmpty` t) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instructions_inttoptr) <- setVar (L.inttoptr x)
  let instruction_store = L.store L.nodeTySyn v' x_unnamed_ptr
  instructions2 <- termToLlvm t
  pure $ instructions1 `List1.prependList`
    (instructions_inttoptr <| instruction_store <| instructions2)

termToLlvm (Unit (VariableNode n vs) `Bind` LAltVar (L.mkLocalId -> x) t) = do
  v <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  (instructions1, vs') <- first concat . List1.unzip <$> mapM valToLlvm vs

  xs <- (|: x) <$> replicateM (List1.length vs') freshUnnamedVar
  let instructions2 =
        snd $ mapAccumL (\v_acc (v, offset, x) -> (L.LocalId x, L.SetVar x $ L.insertvalue v_acc v offset))
        L.Undef $ list1zip3 (v <| vs') (0 :| [1 ..]) xs

  instructions3 <- varLocal x (termToLlvm t)

  pure $ instructions1 `List1.prependList` instructions2 <> instructions3

-- Returning tags are different because they currently need to be boxed
termToLlvm (Unit (Tag tag)) = do
  v <- L.mkLit <$> tagNumLookup tag
  let instruction_insertvalue = L.insertvalue L.Undef v 0
  cont <- view continuationLens
  cont instruction_insertvalue

termToLlvm (Unit (VariableNode n vs)) = do
  v <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  (is1, (vs', v')) <- bimap concat (List1.initLast . (v <|)) . List1.unzip <$> mapM valToLlvm vs
  (v'', is2) <- forAccumM L.Undef (zip vs' [0 ..]) $ \acc (v, offset) -> do
    (x_unnamed, i_insertvalue) <- first L.LocalId <$> setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  cont <- view continuationLens
  let offset = length vs
  is3 <- cont (L.insertvalue v'' v' offset)
  pure $ (is1 ++ is2) `List1.prependList` is3

termToLlvm (Unit (ConstantNode tag vs)) = do
  v <- L.mkLit <$> tagNumLookup tag
  (instructions1, (vs', v')) <- bimap concat (List1.initLast . (v <|)). List1.unzip <$> mapM valToLlvm vs
  (v'', instructions2) <- forAccumM L.Undef (zip vs' [0 ..]) $ \acc (v, offset) -> do
    (x_unnamed, i_insertvalue) <- first L.LocalId <$> setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  let instruction_insertvalue = L.insertvalue v'' v' (length vs)
  instructions3 <- ($ instruction_insertvalue) =<< view continuationLens
  pure $ (instructions1 ++ instructions2) `List1.prependList` instructions3

termToLlvm (App (Def "printf") [v]) = do
  (is, v') <- valToLlvm v
  let format = L.GlobalId $ L.mkGlobalId @String "%d"
      i_call = L.Call L.Fastcc L.Void "@printf" [(L.Ptr, format), (L.I64, v')]
  pure $ is `List1.prependList` (i_call <| L.RetVoid :| [])

-- TODO tail call?
termToLlvm (App (Def (L.mkGlobalId -> f)) vs) = do
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  (instructions1, vs') <- first concat . unzip <$> mapM valToLlvm vs
  let instruction_call = L.Call L.Fastcc returnTy f (zip argsTy vs')
  instructions2 <- ($ instruction_call) =<< view continuationLens
  pure (instructions1 `List1.prependList` instructions2)

termToLlvm (Error TUnreachable) = pure $ List1.singleton L.Unreachable
termToLlvm t = error $ "BAD: " ++ show t

laltToContinuation :: LAlt -> L.Instruction -> LlvmGen (List1 L.Instruction)
laltToContinuation (LAltVar (L.mkLocalId -> x) t) i = (L.SetVar x i <|) <$> varLocal x (termToLlvm t)
laltToContinuation (LAltConstantNode _ (map L.mkLocalId -> xs) t) i = do
  (L.LocalId -> x_unnamed, i_setVar) <- setVar i
  let is_extractvalue = zipWith (\x -> L.SetVar x . L.extractvalue x_unnamed) xs [1 ..]
  let xs' = caseList xs __IMPOSSIBLE__ (:|)
  is <- varLocals xs' (termToLlvm t)
  pure $ (i_setVar :| is_extractvalue) <> is
laltToContinuation (LAltVariableNode (L.mkLocalId -> x) (map L.mkLocalId -> xs) t) i = do
  (L.LocalId -> x_unnamed, i_setVar) <- setVar i
  let is_extractvalue = zipWith (\x -> L.SetVar x . L.extractvalue x_unnamed) (x : xs) [0 ..]
  is <- varLocals (x :| xs) (termToLlvm t)
  pure $ (i_setVar :| is_extractvalue) <> is
laltToContinuation (LAltEmpty t) _ = termToLlvm t

valToLlvm :: Val -> LlvmGen ([L.Instruction], L.Val)
valToLlvm (Var n) = maybe __IMPOSSIBLE__ (([],) . L.LocalId) <$> varLookup n
valToLlvm (Def s) = pure ([], L.LocalId $ L.mkLocalId s)
valToLlvm (Prim _) = __IMPOSSIBLE__
valToLlvm (Lit lit) = pure ([], L.Lit lit)
valToLlvm (VariableNode n vs) = do
  v <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  (instructions1, vs') <- bimap concat (v <|) . List1.unzip <$> mapM valToLlvm vs
  (v', instructions2) <- forAccumM L.Undef (List1.zip vs' (0 :| [1 ..])) $ \acc (v, offset) -> do
    (L.LocalId -> x_unnamed, i_insertvalue) <- setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  pure (instructions1  ++ toList instructions2, v')
valToLlvm (ConstantNode tag vs) = do
  v <- L.mkLit <$> tagNumLookup tag
  (instructions1, vs') <- bimap concat (v <|) . List1.unzip <$> mapM valToLlvm vs
  (v', instructions2) <- forAccumM L.Undef (List1.zip vs' (0 :| [1 ..])) $ \acc (v, offset) -> do
    (L.LocalId -> x_unnamed, i_insertvalue) <- setVar (L.insertvalue acc v offset)
    pure (x_unnamed, i_insertvalue)
  pure (instructions1  ++ toList instructions2, v')
valToLlvm (Tag tag) = ([], ) . L.Lit <$> tagToLlvm tag
valToLlvm Empty = __IMPOSSIBLE__

data LlvmGenEnv = LlvmGenEnv
  { cg_freshUnnamed :: Int
  , cg_freshTagNum  :: Int
  , cg_freshAltNum  :: Int
  , cg_tagNums      :: Map Tag Int
  }

initLlvmGenEnv :: LlvmGenEnv
initLlvmGenEnv = LlvmGenEnv
  { cg_freshUnnamed = 1
  , cg_freshTagNum  = 0
  , cg_freshAltNum  = 0
  , cg_tagNums      = mempty
  }

freshUnnamedLens :: Lens' LlvmGenEnv Int
freshUnnamedLens f cxt = f cxt.cg_freshUnnamed <&> \n -> cxt{cg_freshUnnamed = n}

freshUnnamedVar :: MonadState LlvmGenEnv m => m L.LocalId
freshUnnamedVar = L.mkUnnamed <$> use freshUnnamedLens <* modify (over freshUnnamedLens succ)

freshTagNumLens :: Lens' LlvmGenEnv Int
freshTagNumLens f env = f env.cg_freshTagNum <&> \n -> env{cg_freshTagNum = n}

freshTagNum :: MonadState LlvmGenEnv m => Tag -> m Int
freshTagNum tag = do
  whenJustM (Map.lookup tag <$> use tagNumsLens) __IMPOSSIBLE__
  n <- use freshTagNumLens
  modify $ over freshTagNumLens succ
  modify $ over tagNumsLens $ Map.insert tag n
  pure n

freshAltNumLens :: Lens' LlvmGenEnv Int
freshAltNumLens f env = f env.cg_freshAltNum <&> \n -> env{cg_freshAltNum = n}

freshAltNum :: MonadState LlvmGenEnv m => m Int
freshAltNum = use freshAltNumLens <* modify (over freshAltNumLens succ)

tagNumsLens :: Lens' LlvmGenEnv (Map Tag Int)
tagNumsLens f env = f env.cg_tagNums <&> \n -> env{cg_tagNums = n}

tagNumLookup :: MonadState LlvmGenEnv m => Tag -> m Int
tagNumLookup tag =
  fromMaybeM (freshTagNum tag) $ gets $ Map.lookup tag . (^. tagNumsLens)

data LlvmGenCxt = LlvmGenCxt
  { cg_vars         :: [L.LocalId]
  , cg_globalsTy    :: GlobalsTy
  , cg_continuation :: Continuation
  }

type Continuation = L.Instruction -> LlvmGen (List1 L.Instruction)

type GlobalsTy = Map L.GlobalId ([L.Type], L.Type)

initLlvmGenCxt :: [GrinDefinition] -> GrinDefinition -> LlvmGenCxt
initLlvmGenCxt defs def =
    LlvmGenCxt
      { cg_vars = vars
      , cg_globalsTy = globalsTy
      , cg_continuation = cont
      }
  where
    vars = map L.mkLocalId (reverse def.gr_args)
    globalsTy = mkGlobalTys defs
    cont i = do
      (x_unnamed, i_setVar) <- first L.LocalId <$> setVar i
      pure $ i_setVar <| L.RetNode x_unnamed :| []

lensVars :: Lens' LlvmGenCxt [L.LocalId]
lensVars f cxt = f cxt.cg_vars <&> \xs -> cxt{cg_vars = xs}

varLocals :: MonadReader LlvmGenCxt m => List1 L.LocalId -> m a -> m a
varLocals = foldr (\x f -> varLocal x . f) id

varLocal :: MonadReader LlvmGenCxt m => L.LocalId -> m a -> m a
varLocal x = locally lensVars (x :)

varLookup :: MonadReader LlvmGenCxt m => Int -> m (Maybe L.LocalId)
varLookup n = asks $ varLookup' n

varLookup' :: Int -> LlvmGenCxt -> Maybe L.LocalId
varLookup' n cxt = (cxt ^. lensVars) !!! n

globalsTyLens :: Lens' LlvmGenCxt GlobalsTy
globalsTyLens f cxt = f cxt.cg_globalsTy <&> \ts -> cxt{cg_globalsTy = ts}

globalLookup :: MonadReader LlvmGenCxt m => L.GlobalId -> m (Maybe ([L.Type], L.Type))
globalLookup f = Map.lookup f <$> view globalsTyLens

continuationLens :: Lens' LlvmGenCxt Continuation
continuationLens f cxt = f cxt.cg_continuation <&> \cont -> cxt{cg_continuation = cont}

continuationLocal :: MonadReader LlvmGenCxt m => Continuation -> m a -> m a
continuationLocal cont = locally continuationLens (const cont)

newtype LlvmGen a = LlvmGen{runLlvmGen :: ReaderT LlvmGenCxt (State LlvmGenEnv) a}
  deriving (Functor, Applicative, Monad, MonadState LlvmGenEnv, MonadReader LlvmGenCxt)

tagToLlvm :: Tag -> LlvmGen Literal
tagToLlvm = fmap (LitNat . toInteger) . tagNumLookup

todoLlvm s = pure $ L.Comment ("TODO: " ++ take 30 (prettyShow s)) :| []

setVar :: L.Instruction -> LlvmGen (L.LocalId, L.Instruction)
setVar i = freshUnnamedVar <&> \x -> (x, L.SetVar x i)
