{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ViewPatterns             #-}
module Compiler.Llvm.Compiler (module Compiler.Llvm.Compiler) where

import           Control.DeepSeq                     (NFData)
import           Control.Monad                       (forM, join, mapAndUnzipM,
                                                      replicateM, void, when)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Reader                (MonadReader,
                                                      ReaderT (runReaderT),
                                                      asks, local)
import           Control.Monad.State                 (MonadState, State, StateT,
                                                      evalStateT, gets, modify,
                                                      runState)
import           Data.Bifunctor                      (Bifunctor (bimap, first))
import           Data.Bool                           (bool)
import           Data.Foldable                       (foldrM, toList)
import           Data.Function                       (on)
import           Data.List                           (intercalate, mapAccumL,
                                                      mapAccumR, singleton,
                                                      sortOn, unzip4)
import           Data.List.NonEmpty.Extra            ((|:), (|>))
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import           Data.Tuple.Extra                    (second, swap, uncurry3)
import           GHC.Generics                        (Generic)
import           Prelude                             hiding (drop, (!!))
import qualified Prelude                             as P
import           System.FilePath                     ((<.>), (</>))
import           System.Process.Extra                (readCreateProcess, shell)

import           Agda.Compiler.Backend               hiding (Prim)
import           Agda.Interaction.Options
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal                 (Literal (LitNat))
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.SizedTypes.Utils  (setDebugging, traceM)
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Applicative              (forA)
import           Agda.Utils.Function                 (applyWhen, applyWhenJust,
                                                      applyWhenM)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible               (__IMPOSSIBLE__)
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.List1                    (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1                    as List1
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                    (ifM, mapMaybeM)
import           Control.Applicative                 (Applicative (liftA2))
import           GHC.IO                              (unsafePerformIO)
import           System.Directory.Extra              (removeFile)

import           Compiler.Grin.Grin
import           Compiler.Grin.GrinInterpreter       (interpretGrin,
                                                      printInterpretGrin)
import           Compiler.Grin.GrinTransformations
import           Compiler.Grin.HeapPointsTo
import           Compiler.Grin.Perceus
import qualified Compiler.Llvm.Llvm                  as L
import           Compiler.Treeless.TreelessTransform

import qualified Utils.List1                         as List1
import           Utils.Utils



-- LONG TERM IDEAS AND TODOs
--
-- • Lambda lifting
--
-- • Erase proofs and other unused terms (e.g. second argument of const). #6928
--
-- • Typed GRIN.
--   - Only use layout types for better code generation.
--   - Distinguish between linear and omega modalities (all zero modalities are
--     hopefully erased).
--   - Will probably have big type inference pass which inserts types, specialization,
--     and strictness analysis. Would be intresting to guide the strictness analysis with
--     a cost model based on Hoffmann et al. [2000, 2022].
--
-- • Specialization/Monomorphization
--   - Treat monomorphization, type class dispatch, and specialization the same. Example:
--
--     -- Defintions
--     return : {A : Set} → ⦃M : RawMonad⦄ → A → M A
--     return = ...
--
--     map : {A B : Set} → (A → B) → List A → List B
--     map = ...
--
--     -- Usages
--     return {ℕ} ⦃listMonad⦄ ...
--     map {ℕ} {ℕ} (2 *_) ...
--
--     -- Specialization
--
--     returnListNat : ℕ → List ℕ
--     returnListNat n = n ∷ []
--
--     mapDouble : List ℕ → List ℕ
--     mapDouble (n ∷ ns) = 2 * n ∷ mapDouble ns
--
--   - Singular usage of a specialization should always be inlined.
--     Small specializations should preferably also be inlined
--     (or is it better to do this during late inlining?).
--
--   - Need to balance code size and branching costs. Instead of specializing mapDouble, we can
--     defunctionalize and pattern match on the funciton. Can we extend this to work for
--     parametric and adhoc polymorphism?????
--
--     FIXME maybe not relevant ↓↓↓↓↓
--
--   - Treeless is untyped. So, the monomorphization may need to be part
--     of a treeless-to-GRIN type inference phase. Which means that MAlonzo can not reuse
--     the optimizations.
--   - Big obstacles are higher ranked types and polymorphic recursion. But it
--     should be feasible with interprodural analysis. We may end up doing something
--     similiar to [Brandon, 2023]. However, it would be best to do a "simple" monomorphization
--     (fuctions with the same layout type share one definition) and then implement a more general
--     specializtion pass to avoid branching.
--
-- • Dead code elimination. #4733
--
-- • Incremental/parallel compilation by storing call graph information in
--   interface-like files. Then, there can be "thin" interprodural analysis
--   which performs monomorphization and points-to analysis. These ideas are
--   similiar to LLVM's ThinLTO.
--
--   FIXME figure is wrong
--
--           +            +            +     • Frontend stuff
--           |            |            |
--         .agdai       .agdai       .agdai   (internal syntax + other info)
--           |            |            |
--           +            +            +     • Optionally do any of the following: add call graph information, lambda lift, add type constraints, add points-to equations.
--           |            |            |
--         .agdai       .agdai       .agdai   (maybe treeless)
--           \            |            /
--            +--------.agdao---------+      • Interprodural analysis. Calculate transitive closure (reachability), mono types, and solve points-to equations.
--            |           |           |
--            +           +           +      • Backend stuff. Use some smart diffing algorithm to only recompile affected modules.
--            |           |           |
--          .hs,ll      .hs,ll      .hs,ll
--            |           |           |
--            +           +           +      • GHC / Clang
--            |           |           |
--            .o          .o          .o
--             \          |          /
--              +---------+---------+        • Linking
--                        |
--                   ELF executable
--
-- • Some other GRIN transformations require interprodural analysis e.g. late inlining and arity raising.
--   Look at ThinLTO how they solved it. ThinLTO seems to only be able to do one layer of cross-module
--   inlining. And it is unclear how "imports" work when funA inlines a function funB which require an
--   new import funC.
--
--      module A where    module B where    module C  where
--      open import B     open import C
--      funA = funB       funB = funC       funC = ...
--
-- • Solve integer overflow issues. Maybe use bignum for Nat and an arbirary precision integer
--   for Fin. Also, look into GHC's approach of promoting to bignum.
--
-- • Need to prevent stack overflow either by making things more strict by
--   a demand analysis or/and by optimizing away laziness with deforestation and listlessness
--   (via e.g. arity raising and late inlining).
--
-- • Figure out a data layout that is optimal for Perceus-style memory reuse.
--
-- • Figure out some smart way to identify and flatten/pack trees. If we know that the data
--   structure is packed (with a certain alignment) and we know the pattern (size), then we
--   should be able to optimize it to a O(1) access. However, it is non-trivial to pack an
--   arbirary tree, and I wonder how this will interact with different node layouts.
--
--   ```
--     -- Treeless
--     proj₃ : Vec A 4 → A
--     proj₃ xs₁ =
--       case xs₁ of
--         _ ∷ xs₂ →
--         case xs₂ of
--           _ ∷ xs₃ →
--           case xs₃ of
--             x ∷ _ → x
--
--     -- Semi-optimized GRIN
--     -- 4 sequential pointer dereferences
--     proj₃ xs₁ =
--       fetch [3] xs₁ ; λ xs₂ →
--       fetch [3] xs₂ ; λ xs₃ →
--       fetch [2] xs₃ ; λ x →
--       fetch [2] x   ; λ n →
--       unit (Cnat n)
--
--     -- Optimized GRIN
--     -- Assume packed layout [cons , _ , next , cons , _ , next , cons , (nat n) , next , cons ,  _ , nil]
--     -- Maybe remove next pointers when packing structure?
--     -- Unclear if nested structure also have to be packed. And what happens when we extend arrays.
--     -- Is it possible to do dynamic arrays (reallocate and expand when x% full)?
--     proj₃ xs =
--       fetch [8] xs ; λ → n
--       unit (Cnat n)
--
--   ```
--
-- • Use sized types (BUILTIN) to maybe inline recursive functions, and pack/flatten inductive types.
--
-- • Add proper interop with C.

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

data LlvmOptions = LlvmOptions
  { flagLlvmCompile   :: Bool
  -- , flagLlvmOptFlag   :: String
  , flagLlvmOpt       :: Bool
  , flagLlvmInterpret :: Bool
  , flagLLvmOutput    :: FilePath
  } deriving (Generic)

instance NFData LlvmOptions

newtype LlvmEnv = LlvmEnv
  { envLlvmOpts :: LlvmOptions
  }

data LlvmModuleEnv = LlvmModuleEnv
newtype LlvmModule = LlvmModule [TreelessDefinition]

defaultLlvmOptions :: LlvmOptions
defaultLlvmOptions = LlvmOptions
  { flagLlvmCompile   = False
  -- , flagLlvmOptFlag   = ""
  , flagLlvmOpt       = False
  , flagLlvmInterpret = False
  , flagLLvmOutput    = "program"
  }

llvmCommandLineFlags :: [OptDescr (Flag LlvmOptions)]
llvmCommandLineFlags =
  [ Option []  ["llvm"]
      do NoArg \ o -> pure o{flagLlvmCompile = True}
      do "compile program using the LLVM backend"
  -- , Option []  ["opt-flag"]
  --     do ReqArg (\ f o -> pure o{flagLlvmOptFlag = o.flagLlvmOptFlag ++ " " ++ f}) "OPT-FLAG"
  --     do "give the flag OPT-FLAG to LLVM optimizer"
  , Option ['O'] []
      do NoArg \ o -> pure o{flagLlvmOpt = True}
      do "enable LLVM optimizations"
  , Option []  ["interpret"]
      do NoArg \ o -> pure o{flagLlvmInterpret = True}
      do "interprets the final GRIN code"
  , Option ['o']  []
      do ReqArg (\ f o -> pure o{flagLLvmOutput = f}) "FILE"
      do "set output filename"
  ]

llvmPreCompile :: LlvmOptions -> TCM LlvmEnv
llvmPreCompile = pure . LlvmEnv

-- TODO need to filter Unreachable functions
llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM (Maybe TreelessDefinition)

llvmCompileDef _ _ isMain def = do
  liftIO $ setDebugging True
  definitionToTreeless isMain def



--  defs_grin <- map (updateGrTerm bindNormalisation) <$> treelessToGrin defs_treeless

llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [Maybe TreelessDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ _ defs =
  pure $ LlvmModule $ catMaybes defs



-- myDefs :: MonadFresh Int mf => mf [GrinDefinition]
-- myDefs = do
--   main <-
--     store (cnat $ mkLit 4)         `bindVarM`
--     store (cnat $ mkLit 6)         `bindVarM`
--     App (Def "_+_") [Var 1, Var 0] `bindCnat`
--     printf 0
--
--   let def_main =
--         GrinDefinition
--           { gr_name = "main"
--           , gr_isMain = True
--           , gr_primitive = Nothing
--           , gr_arity = 0
--           , gr_type = Nothing
--           , gr_term = main
--           , gr_args = []
--           , gr_return = Nothing }
--
--   plus <-
--     eval 1 `bindCnatR`
--     eval 2 `bindCnatR`
--     App (Prim PAdd) [Var 2, Var 0] `bindVar`
--     Unit (cnat $ Var 0)
--
--   arg1 <- freshAbs
--   arg2 <- freshAbs
--   ret <- freshAbs
--
--   let def_plus =
--         GrinDefinition
--           { gr_name = "_+_"
--           , gr_isMain = False
--           , gr_primitive = Just PAdd
--           , gr_arity = 2
--           , gr_type = Nothing
--           , gr_term = plus
--           , gr_args = [arg1, arg2]
--           , gr_return = Just ret }
--
--   pure [def_plus, def_main]



llvmPostCompile :: LlvmEnv
                -> IsMain
                -> Map TopLevelModuleName LlvmModule
                -> TCM ()
llvmPostCompile env _ mods = do

  let defs_treeless = concatMap (\(LlvmModule xs) -> xs) (Map.elems mods)
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Treeless"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_treeless

  defs_grin <- mapM (fmap (updateGrTerm bindNormalisation) . treelessToGrin) defs_treeless
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * GRIN"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_grin

  -- FIXME
  -- printInterpretGrin defs_grin

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

  let returnVars = foldr (\ def -> applyWhenJust def.gr_return (def.gr_name `Map.insert`)) mempty defs_grin

  (tagInfo_evalInlining, defs_evalInlining) <- forAccumM tagInfo_pointsTo defs_grin \ tagInfo def -> do
    (tagInfo, t) <- evalInlining absCxt returnVars tagInfo def
    pure (tagInfo, setGrTerm t def)

  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Eval inlining"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_evalInlining
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_evalInlining

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_evalInlining
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Bind normalisation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_bindNormalisation

  let defs_copyPropagation = map (updateGrTerm copyPropagation) defs_bindNormalisation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Copy propagation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_copyPropagation

  liftIO $ putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_evalInlining

  (tagInfo_vectorization, defs_vectorization) <- 
    forAccumM tagInfo_evalInlining defs_copyPropagation \ tagInfo def -> do
      (tagInfo', t) <- vectorization tagInfo def.gr_term
      pure (tagInfo', setGrTerm t def)

  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Vectorization"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_vectorization
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_vectorization

  let defs_caseSimplification = map (updateGrTerm caseSimplification) defs_vectorization
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Case simplification"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_caseSimplification

  let defs_constantPropagation = map (updateGrTerm $ constantPropagation tagInfo_vectorization) defs_caseSimplification
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Constant propagation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_constantPropagation

  let defs_fetchSpecialisation = map (updateGrTerm fetchSpecialisation) defs_constantPropagation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Fetch specialisation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_fetchSpecialisation

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_fetchSpecialisation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Bind normalisation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_bindNormalisation

  let defs_copyPropagation = map (updateGrTerm copyPropagation) defs_bindNormalisation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Copy propagation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_copyPropagation

  let defs_splitFetchOperations = map (updateGrTerm splitFetchOperations) defs_copyPropagation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Split fetch operations"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_splitFetchOperations

  defs_rightHoistFetchOperations <- mapM (lensGrTerm rightHoistFetchOperations) defs_splitFetchOperations
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Right hoist fetch operations"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_rightHoistFetchOperations

  defs_mem <- do
    drop <- mkDrop defs_fetchSpecialisation
    dup <- mkDup
    pure [drop, dup]

  defs_perceus <- mapM perceus defs_rightHoistFetchOperations
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Perceus"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow $ defs_perceus ++ defs_mem) ++ "\n"

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_perceus
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Bind normalisation"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_bindNormalisation) ++ "\n"

  let defs_fetchReuse = map (updateGrTerm fetchReuse) defs_bindNormalisation
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Fetch Reuse"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_fetchReuse) ++ "\n"

  let defs_dropRaising = map (updateGrTerm dropRaising) defs_fetchReuse
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Drop Raising"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_dropRaising) ++ "\n"

  let defs_dupLowering = map (updateGrTerm dupLowering) defs_dropRaising
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Dup Lowering"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_dupLowering) ++ "\n"

  let defs_dupDropFusion = map (updateGrTerm dupDropFusion) defs_dupLowering
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Dup/Drop Fusion"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" (map prettyShow defs_dupDropFusion) ++ "\n"

  when env.envLlvmOpts.flagLlvmInterpret $ printInterpretGrin (defs_dupDropFusion ++ defs_mem)

  -- liftIO $ putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_vectorization

  let (llvm_ir, tagsToInt) = grinToLlvm (defs_bindNormalisation ++ defs_mem) -- defs_rightHoistFetchOperations 
      header =
        unlines
          [ "target triple = \"x86_64-unknown-linux-gnu\""
          , "declare void @printf(ptr, ...)"
          , "declare ptr @malloc(i64)"
          , "declare void @free(ptr)"
          -- FIXME data layout
          , "%Node = type [4 x i64]"
          -- , "%HeapNode = type {i64, %Node}"
          ]
         ++ "@\"%d\" = private constant [4 x i8] c\"%d\\0A\\00\", align 1"

         -- -- debug
         -- ++ unlines
         -- [ "\n@\"sum\" = private constant [9 x i8] c\"sum: %d\\0A\\00\", align  1"
         -- , "@\"downFrom\" = private constant [14 x i8] c\"downFrom: %d\\0A\\00\", align 1"
         -- , "@\"_-_\" = private constant [9 x i8] c\"_-_: %d\\0A\\00\", align 1"
         -- , "@\"_+_\" = private constant [9 x i8] c\"_+_: %d\\0A\\00\", align 1"
         --
         --    @"rc: %d" = private constant [8 x i8] c"rc: %d\0A\00", align 1
         --    @"tag: %d" = private constant [9 x i8] c"tag: %d\0A\00", align 1
         --
         -- ]

      tags_table = "; Tag numbering table:\n" ++ prettyShow (align 20 (map (bimap ((++) "; " . show) pretty . swap) (sortOn snd (Map.toList tagsToInt))))
      render200 = renderStyle Style{ribbonsPerLine = 1.5, mode = PageMode, lineLength = 200}
      defs = intercalate "\n\n" (map (render200 . pretty) llvm_ir)
      program = intercalate "\n\n" [header, tags_table, defs]


  liftIO $ do
    let file_ll = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ ".ll"
    let file_ll_opt = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ "-opt.ll"
    let file_asm_s = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ "-asm.s"
    putStrLn $ "Writing file " ++ file_ll
    writeFile file_ll program


    -- -O3 -flto
    let opt = unwords
          -- Emit optimized LLVM IR (llvm/program_opt.ll)
          [ "clang -o", file_ll_opt, "-S -O3 -emit-llvm", file_ll
          -- Emit Assembly (llvm/program_asm.s)
          , "&&\nclang -o", file_asm_s, "-O3 -fuse-ld=lld -flto -Wl,--lto-emit-asm", file_ll_opt
          -- Linking with LTO
          , "&&\nclang -o", env.envLlvmOpts.flagLLvmOutput, "-O3 -fuse-ld=lld -flto", file_ll_opt
          ]

    -- -O0
    let no_opt = unwords
          [ "clang -o", file_asm_s, "-S -masm=intel", file_ll
          , "&&\nclang -o", env.envLlvmOpts.flagLLvmOutput, "-fuse-ld=lld", file_ll
          ]



    let cmd = if env.envLlvmOpts.flagLlvmOpt then opt else no_opt
    putStrLn "Linking program"
    putStrLn cmd
    void $ readCreateProcess (shell cmd) ""



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
      { gr_name = def.tl_name
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
      -- | getShortName def == "decref"  = ([L.I64], L.Void)
      | otherwise = (replicate def.gr_arity L.I64, L.nodeTySyn)

mkCont :: GrinDefinition -> Continuation
mkCont (getShortName -> "drop") instruction = pure [instruction, L.RetVoid]
mkCont (getShortName -> "dup")  instruction = pure [instruction, L.RetVoid]
-- mkCont (getShortName -> "decref")  instruction = pure [instruction, L.RetVoid]
mkCont _ i = do
  (x_unnamed, i_setVar) <- first L.LocalId <$> setVar i
  pure [i_setVar, L.RetNode x_unnamed]

definitionToLlvm :: GrinDefinition -> LlvmGen L.Instruction
definitionToLlvm def = do
  let f = L.mkGlobalId def.gr_name
  (argsTy, returnTy) <- fromMaybe (error $ "CANT FIND " ++ prettyShow f) <$> globalLookup f
  let args = zip argsTy $ map L.mkLocalId def.gr_args
  L.Define L.Fastcc returnTy f args <$> termToLlvm def.gr_term

-- TODO Refactor
--  • Need some sort of continuations. We have three continuations variants: 
--      1. Set register and branch to another block (%alt_res = instruction; br label %continue)
--      2. Branch to another block (br label %continue)
--      3. Return value (Ret val)
--  • Switch-Phi combinations need to be able to handle multiple labels in the alternatives.
--    Idea: altToLlvm returns its switch label and the last label (and maybe the result register).
--  • Remove returningLocal
--  • Better handling of builtin functions and main
termToLlvm :: Term -> LlvmGen (List1 L.Instruction)
termToLlvm (Case (Var n) t1 alts `BindEmpty` t2) = do
  alt_num <- freshAltNum
  let continue = "continue_" ++ show alt_num
  let cont = const $ pure $ L.Br (L.mkLocalId continue) :| []
  instructions1 <- continuationLocal cont $ returningLocal True $ termToLlvm (Case (Var n) t1 alts)
  instructions2 <- termToLlvm t2
  pure (instructions1 |> L.Label continue instructions2)

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

      mkBlock s x t = do
        t' <- continuationLocal cont $ returningLocal False (termToLlvm t)
        pure (L.Label s t')
        where
        cont i = pure $ L.SetVar x i <|  L.Br (L.mkLocalId continue) :| []


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
  (x_unnamed, instruction_insertvalue) <- first L.LocalId <$> setVar (L.insertvalue v' (L.mkLit 1) 0)
  (x_unnamed_ptr, instruction_malloc) <- setVar (L.malloc L.nodeSize)  -- L.alloca
  let instruction_store = L.store L.nodeTySyn x_unnamed x_unnamed_ptr
      instruction_ptrtoint = L.SetVar x (L.ptrtoint x_unnamed_ptr)
  instructions2 <- varLocal x (termToLlvm t)
  pure $ instructions1 `List1.prependList`
    ([instruction_insertvalue, instruction_malloc, instruction_store, instruction_ptrtoint] <> instructions2)

termToLlvm (FetchOpaqueOffset n offset `Bind` alt)
  | elem @[] offset [0, 1] = fetchToLlvm n offset alt
termToLlvm (FetchOffset _ n offset `Bind` alt)
  | elem @[] offset [1, 2, 3] = fetchToLlvm n offset alt


termToLlvm (FetchOffset _ n 0 `Bind` LAltVar (L.mkLocalId -> x )
           (Case (Var 0) t1 alts `BindEmpty` t2)) = do
  continue <- ("continue_" ++) .  show <$> freshAltNum

  x_ptr <- fromMaybe __IMPOSSIBLE__ <$> varLookup n

  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x_ptr)
  (x_unnamed, instruction_getelemtptr) <- setVar (L.getelementptr x_unnamed_ptr 0)
  let instruction_setVar = L.SetVar x (L.load L.I64 x_unnamed)

  instructions1 <- varLocal x $ continuationLocal
                     (\instruction ->  pure [instruction, L.Br $ L.mkLocalId continue])
                     (termToLlvm $ Case (Var 0) t1 alts)

  instructions2 <- varLocal x_ptr (termToLlvm t2)

  let instruction_continue = L.Label continue instructions2

  pure $
    [ instruction_inttoptr
    , instruction_getelemtptr
    , instruction_setVar ] <>
     (instructions1 |>
      instruction_continue)


termToLlvm (App (Prim prim) [v1, v2] `Bind` alt) = do
  let op = case prim of
        PAdd -> L.add64
        PSub -> L.sub64
        _    -> __IMPOSSIBLE__
  (is1, v1') <- valToLlvm v1
  (is2, v2') <- valToLlvm v2
  List1.prependList (is1 ++ is2) <$> laltToContinuation alt (op v1' v2')

termToLlvm (Unit (VariableNode n vs) `Bind` LAltVar (L.mkLocalId -> x) t) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  instructions1 <- insertValues (pure . List1.singleton . L.SetVar x) tagNum vs
  instructions2 <- varLocal x (termToLlvm t)
  pure (instructions1 <> instructions2)

-- termToLlvm (Unit (Tag tag)) = do
--   tagNum <- L.mkLit <$> tagNumLookup tag
--   cont <- view continuationLens
--   insertValues cont tagNum [Tag tag]

termToLlvm (Unit (VariableNode n vs)) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  cont <- view continuationLens
  insertValues cont tagNum vs

termToLlvm (Unit (ConstantNode tag vs)) = do
  tagNum <- L.mkLit <$> tagNumLookup tag
  cont <- view continuationLens
  insertValues cont tagNum vs

termToLlvm (App (Def "printf") [v]) = do
  (is, v') <- valToLlvm v
  let format = L.GlobalId $ L.mkGlobalId @String "%d"
      i_call = L.Call Nothing L.Fastcc L.Void "@printf" [(L.Ptr, format), (L.I64, v')]
  pure $ is `List1.prependList` (i_call <| L.RetVoid :| [])

termToLlvm (App (Def (L.mkGlobalId -> f)) vs `Bind` alt) = do
  (argsTy, returnTy) <- fromMaybe (error $ "can't find " ++ prettyShow f) <$> globalLookup f
  (instructions, vs') <- first concat <$> mapAndUnzipM valToLlvm vs
  let instruction_call = L.Call Nothing L.Fastcc returnTy f $ zip argsTy vs'
  List1.prependList instructions <$> laltToContinuation alt instruction_call


termToLlvm (App (Def "free") [Var n]) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (x_unnamed, instruction_inttoptr) <- first L.LocalId <$> setVar (L.inttoptr x)
  let instruction_call = L.Call Nothing L.Fastcc L.Void "@free" [(L.Ptr, x_unnamed)]
  instructions2 <- ($ instruction_call) =<< view continuationLens
  pure (instruction_inttoptr <| instructions2)


termToLlvm (App (Def (L.mkGlobalId -> f)) vs) = do
  (argsTy, returnTy) <- fromMaybe __IMPOSSIBLE__ <$> globalLookup f
  (instructions1, vs') <- first concat . unzip <$> mapM valToLlvm vs
  -- If the tail call contains are passed references to new alllocations made in
  -- the current function then Clang is unable to tail call optimize. So, currently
  -- we only uses the tailcall hint `tail` instead of the garantee `musttail`.
  tail <- asks $ flip boolToMaybe L.Tail . cg_returning
  let instruction_call = L.Call tail L.Fastcc returnTy f (zip argsTy vs')
  instructions2 <- ($ instruction_call) =<< view continuationLens
  pure (instructions1 `List1.prependList` instructions2)

-- Important to fetch the reference count and combine it with the rest of the node
-- before updating.
termToLlvm (Update _ _ n v `BindEmpty` t) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed_ptr', instruction_getelementptr) <- setVar (L.getelementptr x_unnamed_ptr 0)
  (x_unnamed, instruction_load) <- first L.LocalId <$> setVar (L.load L.I64 x_unnamed_ptr')
  (x_unnamed', instruction_insertvalue) <- first L.LocalId <$> setVar (L.insertvalue v' x_unnamed 0)
  let instruction_store = L.store L.nodeTySyn x_unnamed' x_unnamed_ptr
  instructions2 <- termToLlvm t
  let instructions =
        [ instruction_inttoptr
        , instruction_getelementptr
        , instruction_load
        , instruction_insertvalue
        , instruction_store
        ]
  pure $ instructions1 `List1.prependList` (instructions <> instructions2)


-- Important to fetch the reference count and combine it with the rest of the node
-- before updating.
termToLlvm (Update _ _ n v) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed_ptr', instruction_getelementptr) <- setVar (L.getelementptr x_unnamed_ptr 0)
  (x_unnamed, instruction_load) <- first L.LocalId <$> setVar (L.load L.I64 x_unnamed_ptr')
  (x_unnamed', instruction_insertvalue) <- first L.LocalId <$> setVar (L.insertvalue v' x_unnamed 0)
  let instruction_store = L.store L.nodeTySyn x_unnamed' x_unnamed_ptr
  let instructions =
        [ instruction_inttoptr
        , instruction_getelementptr
        , instruction_load
        , instruction_insertvalue
        , instruction_store
        ]
  instructions2 <- ($ __IMPOSSIBLE__) =<< view continuationLens
  pure $ instructions1 `List1.prependList` (instructions <> instructions2)



termToLlvm (UpdateOffset n offset v) = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (instructions1, v') <- valToLlvm v
  (x_unnamed_ptr, instruction_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed_ptr', instruction_getelementptr) <- setVar (L.getelementptr x_unnamed_ptr offset)
  let instruction_store = L.store L.I64 v' x_unnamed_ptr'
  instructions2 <- ($ instruction_store) =<< view continuationLens
  pure $
    instructions1 `List1.prependList`
    [ instruction_inttoptr
    , instruction_getelementptr ]
    <> instructions2

termToLlvm (Error TUnreachable) = pure $ List1.singleton L.Unreachable
termToLlvm t = error $ "BAD:\n" ++ show t-- render (nest 4 $ pretty t)

fetchToLlvm :: Int -> Int -> LAlt -> LlvmGen (List1 L.Instruction)
fetchToLlvm n offset alt = do
  x <- fromMaybe __IMPOSSIBLE__ <$> varLookup n
  (x_unnamed_ptr, i_inttoptr) <- setVar (L.inttoptr x)
  (x_unnamed, i_getelemtptr) <- setVar (L.getelementptr x_unnamed_ptr offset)
  List1.prependList [i_inttoptr, i_getelemtptr] <$> laltToContinuation alt (L.load L.I64 x_unnamed)

laltToContinuation :: LAlt -> L.Instruction -> LlvmGen (List1 L.Instruction)
laltToContinuation (LAltVar (L.mkLocalId -> x) t) i = (L.SetVar x i <|) <$> varLocal x (termToLlvm t)
laltToContinuation (LAltConstantNode _ (map L.mkLocalId -> xs) t) instruction = do
  (unnamed, instruction_setVar) <- first L.LocalId <$> setVar instruction
  let instructions_extractvalue = zipWith (\x -> L.SetVar x . L.extractvalue unnamed) xs [2 ..]
  instructions <- varLocals xs (termToLlvm t)
  pure $ (instruction_setVar :| instructions_extractvalue) <> instructions

laltToContinuation (LAltVariableNode (L.mkLocalId -> x) (map L.mkLocalId -> xs) t) i = do
  (unnamed, i_setVar) <- first L.LocalId <$> setVar i
  let instructions_extractvalue = List1.zipWith (\x -> L.SetVar x . L.extractvalue unnamed) (x :| xs) [1 ..]
  is <- varLocals (x : xs) (termToLlvm t)
  pure $ i_setVar <| (instructions_extractvalue <> is)
laltToContinuation (LAltEmpty t) instruction = (instruction <|) <$> termToLlvm t

valToLlvm :: Val -> LlvmGen ([L.Instruction], L.Val)
valToLlvm (Var n) = maybe __IMPOSSIBLE__ (([],) . L.LocalId) <$> varLookup n
valToLlvm (Def s) = pure ([], L.LocalId $ L.mkLocalId s)
valToLlvm (Prim _) = __IMPOSSIBLE__
valToLlvm (Lit lit) = pure ([], L.Lit lit)
valToLlvm (VariableNode n vs) = do
  tagNum <- maybe __IMPOSSIBLE__ L.LocalId <$> varLookup n
  let cont instruction = freshUnnamedVar <&> \unnamed -> List1.singleton (L.SetVar unnamed instruction)
  instructions <- insertValues cont tagNum vs
  unnamed <- L.LocalId . L.mkLocalId . pred <$> use freshUnnamedLens
  pure (toList instructions, unnamed)
valToLlvm (ConstantNode tag vs) = do
  tagNum <- L.mkLit <$> tagNumLookup tag
  let cont instruction = freshUnnamedVar <&> \unnamed -> List1.singleton (L.SetVar unnamed instruction)
  instructions <- insertValues cont tagNum vs
  unnamed <- L.LocalId . L.mkLocalId . pred <$> use freshUnnamedLens
  pure (toList instructions, unnamed)
valToLlvm (Tag tag) = ([], ) . L.Lit <$> tagToLlvm tag
valToLlvm Empty = __IMPOSSIBLE__

-- values should not include tag nor reference count
insertValues :: Continuation -> L.Val -> [Val] -> LlvmGen (List1 L.Instruction)
insertValues cont tagNum vs = do
  (instructions1, vs') <- first concat . unzip <$> mapM valToLlvm vs
  let (vs, v) = List1.initLast (tagNum :| vs')
      (offsets, offset) = List1.initLast [1 .. length vs' + 1]
  -- TODO replace undef with poison (LLVM devs are trying to remove undef)
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
  , cg_returning    :: Bool
  }

type Continuation = L.Instruction -> LlvmGen (List1 L.Instruction)

type GlobalsTy = Map L.GlobalId ([L.Type], L.Type)

initLlvmGenCxt :: [GrinDefinition] -> GrinDefinition -> LlvmGenCxt
initLlvmGenCxt defs def = LlvmGenCxt
  { cg_vars         = map L.mkLocalId (reverse def.gr_args)
  , cg_globalsTy    = mkGlobalTys defs
  , cg_continuation = mkCont def
  , cg_returning    = True }

varsLens :: Lens' LlvmGenCxt [L.LocalId]
varsLens f cxt = f cxt.cg_vars <&> \xs -> cxt{cg_vars = xs}

varLocals :: MonadReader LlvmGenCxt m => [L.LocalId] -> m a -> m a
varLocals = foldr (\x f -> varLocal x . f) id

varLocal :: MonadReader LlvmGenCxt m => L.LocalId -> m a -> m a
varLocal x = locally varsLens (x :)

varLookup :: MonadReader LlvmGenCxt m => Int -> m (Maybe L.LocalId)
varLookup n = asks $ varLookup' n

varLookup' :: Int -> LlvmGenCxt -> Maybe L.LocalId
varLookup' n cxt = (cxt ^. varsLens) !!! n

globalsTyLens :: Lens' LlvmGenCxt GlobalsTy
globalsTyLens f cxt = f cxt.cg_globalsTy <&> \ts -> cxt{cg_globalsTy = ts}

globalLookup :: MonadReader LlvmGenCxt m => L.GlobalId -> m (Maybe ([L.Type], L.Type))
globalLookup f = Map.lookup f <$> view globalsTyLens

continuationLens :: Lens' LlvmGenCxt Continuation
continuationLens f cxt = f cxt.cg_continuation <&> \cont -> cxt{cg_continuation = cont}

continuationLocal :: MonadReader LlvmGenCxt m => Continuation -> m a -> m a
continuationLocal cont = locally continuationLens (const cont)

returningLocal :: MonadReader LlvmGenCxt m => Bool -> m a -> m a
returningLocal x = local $ \cxt -> cxt{cg_returning = x}

newtype LlvmGen a = LlvmGen{runLlvmGen :: ReaderT LlvmGenCxt (State LlvmGenEnv) a}
  deriving (Functor, Applicative, Monad, MonadState LlvmGenEnv, MonadReader LlvmGenCxt)

tagToLlvm :: Tag -> LlvmGen Literal
tagToLlvm = fmap (LitNat . toInteger) . tagNumLookup

todoLlvm s = pure $ L.Comment ("TODO: " ++ take 30 (prettyShow s)) :| []

setVar :: L.Instruction -> LlvmGen (L.LocalId, L.Instruction)
setVar i = freshUnnamedVar <&> \x -> (x, L.SetVar x i)
