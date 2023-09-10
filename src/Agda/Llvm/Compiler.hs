{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ViewPatterns             #-}
module Agda.Llvm.Compiler (module Agda.Llvm.Compiler) where

import           Control.DeepSeq                (NFData)
import           Control.Monad                  (forM, mapAndUnzipM, replicateM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (MonadReader, local,
                                                 ReaderT (runReaderT), asks)
import           Control.Monad.State            (MonadState, State, StateT,
                                                 evalStateT, gets, modify,
                                                 runState)
import           Data.Bifunctor                 (Bifunctor (bimap, first))
import           Data.Foldable                  (foldrM, toList)
import           Data.Function                  (on)
import           Data.List                      (intercalate, singleton, sortOn,
                                                 unzip4, mapAccumR, mapAccumL)
import           Data.List.NonEmpty.Extra       ((|:), (|>))
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Data.Tuple.Extra               (second, swap, uncurry3)
import           GHC.Generics                   (Generic)
import           Prelude                        hiding (drop, (!!))

import           Agda.Compiler.Backend          hiding (Prim)
import           Agda.Interaction.Options
import           Agda.Llvm.Grin
import           Agda.Llvm.GrinInterpreter      (interpretGrin, printInterpretGrin)
import           Agda.Llvm.GrinTransformations
import           Agda.Llvm.HeapPointsTo
import qualified Agda.Llvm.Llvm                 as L
import           Agda.Llvm.Perceus              
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
import Agda.Utils.Monad (ifM)
import Agda.Utils.Applicative (forA)
import Agda.Utils.Function (applyWhen, applyWhenM)
import Control.Applicative (Applicative(liftA2))
import System.Directory.Extra (removeFile)



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



myDefs :: MonadFresh Int mf => mf [GrinDefinition]
myDefs = do 
  main <- 
    store (cnat $ mkLit 4)         `bindVarM`
    store (cnat $ mkLit 6)         `bindVarM`
    App (Def "_+_") [Var 1, Var 0] `bindCnat`
    printf 0

  let def_main = 
        GrinDefinition
          { gr_name = "main"
          , gr_isMain = True
          , gr_primitive = Nothing
          , gr_arity = 0
          , gr_type = Nothing
          , gr_term = main
          , gr_args = []
          , gr_return = Nothing }

  plus <- 
    eval 1 `bindCnatR` 
    eval 2 `bindCnatR` 
    App (Prim PAdd) [Var 2, Var 0] `bindVar`
    Unit (cnat $ Var 0)

  arg1 <- freshAbs
  arg2 <- freshAbs
  ret <- freshAbs

  let def_plus = 
        GrinDefinition
          { gr_name = "_+_"
          , gr_isMain = False
          , gr_primitive = Just PAdd
          , gr_arity = 2
          , gr_type = Nothing
          , gr_term = plus
          , gr_args = [arg1, arg2]
          , gr_return = Just ret }

  pure [def_plus, def_main]
  
  

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

  -- defs_grin <- myDefs
  defs_grin <- map (updateGrTerm normalise) <$> treelessToGrin defs_treeless
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

  -- printInterpretGrin defs_inlineEval

  let defs_normalize = map (updateGrTerm normalise) defs_inlineEval
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalize

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_normalize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  defs_specializeUpdate <- mapM (specializeUpdate absCxt) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Specialize Update"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_specializeUpdate

  let defs_normalize = map (updateGrTerm normalise) defs_specializeUpdate
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalize

  defs_vectorize <- mapM (lensGrTerm $ vectorize tagInfo_inlineEval) defs_normalize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Vectorization"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_vectorize

  let defs_simplifyCase = map (updateGrTerm simplifyCase) defs_vectorize
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Simplify case"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_simplifyCase

  let defs_splitFetch = map (updateGrTerm splitFetch) defs_simplifyCase
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Split fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_splitFetch

  let defs_leftUnitLaw = map (updateGrTerm leftUnitLaw) defs_splitFetch
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Left unit law"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_leftUnitLaw

  defs_rightHoistFetch <- mapM (lensGrTerm rightHoistFetch) defs_leftUnitLaw
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Right hoist fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_rightHoistFetch

  printInterpretGrin defs_rightHoistFetch

  def_dup <- mkDup
  def_drop <- mkDrop defs_rightHoistFetch
  defs_perceus <- (++ [def_drop, def_dup]) . map (updateGrTerm normalise) <$> mapM perceus defs_rightHoistFetch
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Perceus"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_perceus) ++ "\n"

  liftIO $ removeFile "trace.log"
  printInterpretGrin defs_perceus

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


  let (llvm_ir, tagsToInt) = grinToLlvm defs_perceus
      header =
        unlines
          [ "target triple = \"x86_64-unknown-linux-gnu\""
          , "declare void @printf(ptr, ...)"
          , "declare ptr @malloc(i64)"
          , "declare void @free(ptr)"
          -- TODO In all instances except Cnat the arguments are points and thus
          --      only need 8 bits. Using 64 bits for pointers are wastefull.
          --
          -- Optimal layout? (Cnat will occupy all args)
          --
          --          88 bits
          -- ----------------------------
          -- | tag | rc | arg1 ... arg8 |
          -- ----------------------------
          --   16    8        8*8
          --
          -- Current layout:
          --           256 bits
          -- ---------------------------------------
          -- | tag | rc | arg1 | arg2 |
          -- ---------------------------------------
          --   64    64    64     64
          , "%Node = type [4 x i64]" ]
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
-- • Reuse evaluated variables (partially done)
-- • Fill in rest of the patterns

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions
-- • Saturated constructors


-- TODO: remember to raise when ever we create a new abstraction that doesn't exist in treeless
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

termToGrinR :: forall mf. MonadFresh Int mf => TTerm -> GrinGen mf Term
termToGrinR (TCase n CaseInfo{caseType} t alts) =
  caseMaybeM (applyEvaluatedOffset n)
    mkEval'
    reuseEval
  where
    mkEval' = case caseType of
      -- Do not keep track evaluated naturals because it causes trouble 
      -- with unboxed/boxed types.
      -- TODO keep track when strictness/demand-analysis and general unboxing is 
      --      implemented.
      CTNat -> do
        (t', alts') <- 
          localEvaluatedNoOffsets 2 $ 
          liftA2 (,) (termToGrinR $ raise 2 t) (mapM altToGrin $ raise 2 alts)
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

-- | 𝓡 [x + y] = eval 1 ; λ Cnat x0 →
--               eval 1 ; λ Cnat x1 →
--               add 1 0 ; λ x2 →
--               unit 0
termToGrinR (TApp (TPrim prim) vs) = do
  isPrimWrapper <- asks $ (Just prim ==) . tl_primitive . definition
  if isPrimWrapper then do
    t <- forceValues (App (Prim prim)) vs
    t `bindVar` Unit (cnat $ Var 0)
  else do
    f_wrapper <- asks (maybe __IMPOSSIBLE__ Def . lookup prim . primitives)
    App f_wrapper <$> mapM operandToGrin vs
    
  where
    -- TODO wrap unit Cnat
  forceValues :: MonadFresh Int mf => ([Val] -> Term) -> [TTerm] -> GrinGen mf Term
  forceValues mkT vs = do
    (mevals, vs') <- foldrM step (Nothing, []) vs
    case mevals of
      Just evals ->  bindEnd evals <$> laltConstantNode natTag (mkT vs')
      Nothing -> pure (mkT vs')
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
            Just evals -> eval n `bindCnat` raise 2 evals
            Nothing -> pure (eval n)
          
          pure (Just evals', raise 2 vs `snoc` Var 0)
    step _ _ = __IMPOSSIBLE__

-- | 𝓡 [foo x y] = foo x y
termToGrinR (TApp (TDef q) vs) = do
  let call = App (Def (prettyShow q)) <$> mapM operandToGrin vs
  shortName <- asks $ List1.last . list1splitOnDots . tl_name . definition
  applyWhen (shortName == "main") (`bindCnatL` printf 0) call


-- TODO think of a nicer way to handle handle functions that don't return
--      a node (either an unboxed value or `()`)
termToGrinR (TLit lit) = do 
  shortName <- asks $ List1.last . list1splitOnDots . tl_name . definition
  if shortName == "main" then
    case lit of
      LitNat n -> pure $ printf (fromInteger n)
      _ -> __IMPOSSIBLE__
  else
    pure $ Unit $ cnat (Lit lit)


-- 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
termToGrinR (TApp (TCon q) vs) =
  caseList vs
    (pure (Unit (Tag (FTag name 0))))
    (\v vs -> Unit . constantCNode name . toList <$> mapM operandToGrin  (v :| vs))
  where
  name = prettyShow q

-- 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ x → 𝓡 [t2]
termToGrinR (TLet t1 t2) = termToGrinC t1 `bindVarM` localEvaluatedNoOffset (termToGrinR t2)
-- Always return a node
termToGrinR (TCon q) = pure (Unit (constantCNode (prettyShow q) []))
termToGrinR (TError TUnreachable) = pure unreachable
termToGrinR (TVar n) = maybe (eval n) (Unit . Var) <$> applyEvaluatedOffset n

termToGrinR t = error $ "TODO: " ++ take 40 (show t)


termToGrinC (TApp f []) = error "TODO CAF"

--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
termToGrinC (TApp (TPrim prim) (v : vs)) = do
  name <- asks (fromMaybe __IMPOSSIBLE__ . lookup prim . primitives)
  loc <- freshLoc
  appToGrinC (Store loc . constantFNode name . toList) (v :| vs)
termToGrinC (TApp (TDef q) (v : vs)) = do
  loc <- freshLoc
  appToGrinC (Store loc . constantFNode (prettyShow q) . toList) (v :| vs)
termToGrinC (TApp (TCon q) (v : vs)) = do
  loc <- freshLoc
  appToGrinC (Store loc . constantCNode (prettyShow q) . toList) (v :| vs)
termToGrinC (TCon q) = do 
  loc <- freshLoc
  pure $ Store loc (constantCNode (prettyShow q) [])
termToGrinC (TLit (LitNat n)) = do 
  loc <- freshLoc
  pure $ Store loc (ConstantNode natTag (mkLit 1) [mkLit n])


termToGrinC t = error $ "TODO: " ++ take 40 (show t)

appToGrinC :: MonadFresh Int mf => (List1 Val -> Term) -> List1 TTerm -> mf Term
appToGrinC mkT vs = foldrM step (mkT vs') stores
  where
  step s t = do 
    loc <- freshLoc
    s loc `bindVar` t

  ((_, stores), vs') = mapAccumR mkStore (0, []) vs

  mkStore (n, stores) (TLit lit) = ((n + 1, store : stores), Var n)
    where
    store loc = Store loc (ConstantNode natTag (mkLit 1) [Lit lit])
  mkStore (n, stores) (TVar m) = ((n, stores), Var (m + n))
  mkStore _ t = error $ "MKSTORE: " ++ show t


altToGrin :: MonadFresh Int mf => TAlt -> GrinGen mf CAlt
altToGrin (TACon q arity t) = do
    -- GRIN adds an extra abstraction for the reference count
    t' <- localEvaluatedNoOffsets (arity + 1) (termToGrinR $ raiseFrom arity 1 t)
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
  local $ \cxt -> cxt{evaluatedOffsets = updateAt n (const (Just offset)) (cxt.evaluatedOffsets)}

localEvaluatedNoOffset :: MonadReader GrinGenCxt m => m a -> m a
localEvaluatedNoOffset = localEvaluatedNoOffsets 1

localEvaluatedNoOffsets :: MonadReader GrinGenCxt m => Int -> m a -> m a
localEvaluatedNoOffsets n =
  local $ \cxt -> cxt{evaluatedOffsets = replicate n Nothing ++ cxt.evaluatedOffsets}

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var

-----------------------------------------------------------------------
-- * LLVM code generation
-----------------------------------------------------------------------

grinToLlvm :: [GrinDefinition] -> ([L.Instruction], Map Tag Int)
grinToLlvm defs = second cg_tagNums $ runState (mapM step defs) initLlvmGenEnv
  where 
  step def = 
    (freshUnnamedLens .= 1) *>
    runReaderT (runLlvmGen $ definitionToLlvm def) (initLlvmGenCxt defs def)


mkGlobalTys :: [GrinDefinition] -> Map L.GlobalId ([L.Type], L.Type)
mkGlobalTys defs =
  Map.insert "@free" ([L.Ptr], L.Void) $ Map.fromList $ map go defs
  where
  go def = (L.mkGlobalId def.gr_name, types)
    where
    types
      | getShortName def == "main" = ([], L.Void)
      | getShortName def == "drop" = ([L.I64], L.Void)
      | getShortName def == "dup"  = ([L.I64], L.Void)
      | otherwise = (replicate def.gr_arity L.I64, L.nodeTySyn)

mkCont :: GrinDefinition -> Continuation
mkCont (getShortName -> "drop") _ = pure [L.RetVoid]
mkCont (getShortName -> "dup")  _ = pure [L.RetVoid]
mkCont _ i = do
  (x_unnamed, i_setVar) <- first L.LocalId <$> setVar i
  pure [i_setVar, L.RetNode x_unnamed]

definitionToLlvm :: GrinDefinition -> LlvmGen L.Instruction
definitionToLlvm def = do
  let f = L.mkGlobalId def.gr_name
  (argsTy, returnTy) <- fromMaybe (error $ "CANT FIND " ++ prettyShow f) <$> globalLookup f
  let args = zip argsTy $ map L.mkLocalId def.gr_args
  L.Define L.Fastcc returnTy f args <$> termToLlvm def.gr_term

-- TODO make it so that unit is the only term which calls to continuation. And make 
--      both main and drop return `unit ()`
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
    ([instruction_malloc, instruction_store, instruction_ptrtoint] <> instructions2)

termToLlvm (FetchOpaqueOffset n offset `Bind` alt) 
  | elem @[] offset [0, 1] = fetchToLlvm n offset alt
termToLlvm (FetchOffset _ n offset `Bind` alt) 
  | elem @[] offset [1, 2, 3] = fetchToLlvm n offset alt

termToLlvm (App (Prim prim) [v1, v2] `Bind` alt) = do
  let op = case prim of
        PAdd -> L.add64
        PSub -> L.sub64
        _    -> __IMPOSSIBLE__
  (is1, v1') <- valToLlvm v1
  (is2, v2') <- valToLlvm v2
  List1.prependList (is1 ++ is2) <$> laltToContinuation alt (op v1' v2')

termToLlvm (Unit (VariableNode n v vs) `Bind` LAltVar (L.mkLocalId -> x) t) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  instructions1 <- insertValues (pure . List1.singleton . L.SetVar x) tagNum (v :| vs)
  instructions2 <- varLocal x (termToLlvm t)
  pure (instructions1 <> instructions2)

termToLlvm (Unit (Tag tag)) = do
  tagNum <- L.mkLit <$> tagNumLookup tag
  cont <- view continuationLens
  insertValues cont tagNum [Tag tag]

termToLlvm (Unit (VariableNode n v vs)) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  cont <- view continuationLens
  insertValues cont tagNum (v :| vs)

termToLlvm (Unit (ConstantNode tag v vs)) = do
  tagNum <- L.mkLit <$> tagNumLookup tag
  cont <- view continuationLens
  insertValues cont tagNum (v :| vs)

termToLlvm (App (Def "printf") [v]) = do
  (is, v') <- valToLlvm v
  let format = L.GlobalId $ L.mkGlobalId @String "%d"
      i_call = L.Call L.Fastcc L.Void "@printf" [(L.Ptr, format), (L.I64, v')]
  pure $ is `List1.prependList` (i_call <| L.RetVoid :| [])

termToLlvm (App (Def (L.mkGlobalId -> f)) vs `Bind` alt) = do
  (instructions, vs') <- first concat <$> mapAndUnzipM valToLlvm vs
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  let i_call = L.Call L.Fastcc returnTy f $ zip argsTy vs'
  List1.prependList instructions <$> laltToContinuation alt i_call

-- TODO tail call
termToLlvm (App (Def "free" ) [Var n]) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (v_unnamed_ptr, instruction_inttoptr) <- first L.LocalId <$> setVar (L.inttoptr x)
  let instruction_call = L.Call L.Fastcc L.Void "@free" [(L.Ptr, v_unnamed_ptr)]
  -- LLVM is stupid and doesn't let you tail call in a switch. Hopefully
  -- the optimizer will tail call it when translating to native.
      instruction_ret  = L.Ret L.Void Nothing
  pure
    [ instruction_inttoptr
    , instruction_call
    , instruction_ret ]

termToLlvm (App (Def (L.mkGlobalId -> f)) vs) = do
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  (instructions1, vs') <- first concat . unzip <$> mapM valToLlvm vs
  let instruction_call = L.Call L.Fastcc returnTy f (zip argsTy vs')
  instructions2 <- ($ instruction_call) =<< view continuationLens
  pure (instructions1 `List1.prependList` instructions2)


termToLlvm (Dup n `BindEmpty` t) = do
  instructions1 <- termToLlvmChangeRc n (`L.add64` L.mkLit 1)
  instructions2 <- termToLlvm t
  pure (instructions1 <> instructions2)

termToLlvm (UpdateTag _ n v `BindEmpty` t) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instructions_inttoptr) <- setVar (L.inttoptr x)
  let instruction_store = L.store L.nodeTySyn v' x_unnamed_ptr
  instructions2 <- termToLlvm t
  pure $ (instructions1 |: instructions_inttoptr |> instruction_store) <> instructions2


termToLlvm (UpdateOffset n offset v) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v 
  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed_ptr', instruction_getelementptr) <- setVar (L.getelementptr x_unnamed_ptr offset)
  let instruction_store = L.store L.I64 v' x_unnamed_ptr'
  instructions2 <- ($ __IMPOSSIBLE__) =<< view continuationLens
  pure $ 
    instructions1 `List1.prependList`
    [ instruction_inttoptr
    , instruction_getelementptr
    , instruction_store ]
    <> instructions2
  

termToLlvm (Error TUnreachable) = pure $ List1.singleton L.Unreachable
termToLlvm t = error $ "BAD: " ++ show t

fetchToLlvm :: Int -> Int -> LAlt -> LlvmGen (List1 L.Instruction)
fetchToLlvm n offset alt = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (x_unnamed_ptr, i_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed, i_getelemtptr) <- setVar (L.getelementptr x_unnamed_ptr offset)
  List1.prependList [i_inttoptr, i_getelemtptr] <$> laltToContinuation alt (L.load L.I64 x_unnamed)


termToLlvmChangeRc :: Int -> (L.Val -> L.Instruction) -> LlvmGen (List1 L.Instruction)
termToLlvmChangeRc n operation = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed, instruction_getelementptr) <- setVar (L.getelementptr x_unnamed_ptr 1)
  -- FIXME not 64 bits
  (v_unnamed_rc, instruction_load) <- first L.LocalId <$> setVar (L.load L.I64 x_unnamed)
  (v_unnamed_rc', instruction_operation) <- first L.LocalId <$> setVar (operation v_unnamed_rc)
  let instruction_store = L.store L.I64 v_unnamed_rc' x_unnamed_ptr
  pure
    [ instruction_inttoptr
    , instruction_getelementptr
    , instruction_load
    , instruction_operation
    , instruction_store ]


laltToContinuation :: LAlt -> L.Instruction -> LlvmGen (List1 L.Instruction)
laltToContinuation (LAltVar (L.mkLocalId -> x) t) i = (L.SetVar x i <|) <$> varLocal x (termToLlvm t)
laltToContinuation (LAltConstantNode _ (L.mkLocalId -> x) (map L.mkLocalId -> xs) t) instruction = do
  (unnamed, instruction_setVar) <- first L.LocalId <$> setVar instruction
  let instructions_extractvalue = List1.zipWith (\x -> L.SetVar x . L.extractvalue unnamed) (x :| xs) [1 ..]
  instructions <- varLocals (x :| xs) (termToLlvm t)
  pure $ (instruction_setVar <| instructions_extractvalue) <> instructions

laltToContinuation (LAltVariableNode (L.mkLocalId -> x1) (L.mkLocalId -> x2) (map L.mkLocalId -> xs) t) i = do
  (unnamed, i_setVar) <- first L.LocalId <$> setVar i
  let instructions_extractvalue = List1.zipWith (\x -> L.SetVar x . L.extractvalue unnamed) (x1 <| x2 :| xs) [0 ..]
  is <- varLocals (x1 <| x2 :| xs) (termToLlvm t)
  pure $ i_setVar <| (instructions_extractvalue <> is)
laltToContinuation (LAltEmpty t) _ = termToLlvm t

valToLlvm :: Val -> LlvmGen ([L.Instruction], L.Val)
valToLlvm (Var n) = maybe __IMPOSSIBLE__ (([],) . L.LocalId) <$> varLookup n
valToLlvm (Def s) = pure ([], L.LocalId $ L.mkLocalId s)
valToLlvm (Prim _) = __IMPOSSIBLE__
valToLlvm (Lit lit) = pure ([], L.Lit lit)
valToLlvm (VariableNode n v vs) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  let cont instruction = freshUnnamedVar <&> \unnamed -> List1.singleton (L.SetVar unnamed instruction)
  instructions <- insertValues cont tagNum (v :| vs)
  unnamed <- L.LocalId . L.mkLocalId . pred <$> use freshUnnamedLens
  pure (toList instructions, unnamed)
valToLlvm (ConstantNode tag v vs) = do
  tagNum <- L.mkLit <$> tagNumLookup tag
  let cont instruction = freshUnnamedVar <&> \unnamed -> List1.singleton (L.SetVar unnamed instruction)
  instructions <- insertValues cont tagNum (v :| vs)
  unnamed <- L.LocalId . L.mkLocalId . pred <$> use freshUnnamedLens
  pure (toList instructions, unnamed)
valToLlvm (Tag tag) = ([], ) . L.Lit <$> tagToLlvm tag
valToLlvm Empty = __IMPOSSIBLE__

-- values should not include tag nor reference count
insertValues :: Continuation -> L.Val -> List1 Val -> LlvmGen (List1 L.Instruction)
insertValues cont tagNum vs = do
  (instructions1, vs') <- first concat . List1.unzip <$> mapM valToLlvm vs
  let (vs, v) = List1.initLast ([tagNum] <> vs')
      (offsets, offset) = List1.initLast [0 .. length vs]
  (acc, instructions2) <- mapAccumM insertValue L.Undef (zip vs offsets)
  instructions3 <- cont (L.insertvalue acc v offset)
  pure (List1.prependList (instructions1 <> instructions2) instructions3)
  where
  insertValue acc (v, offset) = first L.LocalId <$> setVar (L.insertvalue acc v offset)

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
initLlvmGenCxt defs def = LlvmGenCxt
  { cg_vars         = map L.mkLocalId (reverse def.gr_args)
  , cg_globalsTy    = mkGlobalTys defs
  , cg_continuation = mkCont def }

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
