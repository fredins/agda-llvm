{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Compiler.Llvm.Compiler (module Compiler.Llvm.Compiler) where

import           Control.DeepSeq                     (NFData)
import           Control.Monad                       (void, when)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Bifunctor                      (bimap)
import           Data.List                           (intercalate, mapAccumL,
                                                      sortOn)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Tuple.Extra                    (swap)
import           GHC.Generics                        (Generic)
import           Prelude                             hiding (drop, (!!))
import           System.FilePath                     ((</>))
import           System.Process.Extra                (readCreateProcess, shell)

import           Agda.Compiler.Backend               hiding (Prim)
import           Agda.Interaction.Options
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.SizedTypes.Utils  (setDebugging)
import           Agda.Utils.Function                 (applyWhenJust)
import           Agda.Utils.Maybe

import           Compiler.Grin.Grin
import           Compiler.Grin.GrinInterpreter       (printInterpretGrin)
import           Compiler.Grin.GrinTransformations
import           Compiler.Grin.HeapPointsTo
import           Compiler.Grin.Perceus
import           Compiler.Treeless.TreelessTransform

import           Compiler.Grin.Codegen               (treelessToGrin)
import           Compiler.Llvm.Codegen               (grinToLlvm)
import           Utils.Utils
import System.Directory.Extra (createDirectoryIfMissing)



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

  defs_grin <- mapM (fmap (updateGrTerm bindNormalisation) . treelessToGrin) defs_treeless

  -- FIXME
  -- printInterpretGrin defs_grin

  let (absCxtEqs, absCxt, share) = heapPointsTo defs_grin
  let tagInfo_pointsTo = initTagInfo absCxt
  let returnVars = foldr (\ def -> applyWhenJust def.gr_return (def.gr_name `Map.insert`)) mempty defs_grin

  -- TODO 
  -- Process each definition at a time and start using reportSDoc

  (tagInfo_evalInlining, defs_evalInlining) <- forAccumM tagInfo_pointsTo defs_grin \ tagInfo def -> do
    (tagInfo, t) <- evalInlining absCxt returnVars tagInfo def
    pure (tagInfo, setGrTerm t def)

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_evalInlining

  let defs_copyPropagation = map (updateGrTerm copyPropagation) defs_bindNormalisation

  (tagInfo_vectorization, defs_vectorization) <-
    forAccumM tagInfo_evalInlining defs_copyPropagation \ tagInfo def -> do
      (tagInfo', t) <- vectorization tagInfo def.gr_term
      pure (tagInfo', setGrTerm t def)

  let defs_caseSimplification = map (updateGrTerm caseSimplification) defs_vectorization

  let defs_constantPropagation = map (updateGrTerm $ constantPropagation tagInfo_vectorization) defs_caseSimplification

  let defs_fetchSpecialisation = map (updateGrTerm fetchSpecialisation) defs_constantPropagation

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_fetchSpecialisation

  let defs_copyPropagation = map (updateGrTerm copyPropagation) defs_bindNormalisation

  let defs_splitFetchOperations = map (updateGrTerm splitFetchOperations) defs_copyPropagation

  defs_rightHoistFetchOperations <- mapM (lensGrTerm rightHoistFetchOperations) defs_splitFetchOperations

  def_drop <- bindingReturningCase =<< mkDrop defs_fetchSpecialisation

  defs_perceus <- mapM perceus defs_rightHoistFetchOperations

  let defs_bindNormalisation = map (updateGrTerm bindNormalisation) defs_perceus

  let defs_fetchReuse = map (updateGrTerm fetchReuse) defs_bindNormalisation

  let defs_dropRaising = map (updateGrTerm dropRaising) defs_fetchReuse

  let defs_dupLowering = map (updateGrTerm dupLowering) defs_dropRaising

  let defs_dupDropFusion = map (updateGrTerm dupDropFusion) defs_dupLowering

  defs_bindingReturningCase <- mapM bindingReturningCase defs_dupDropFusion

  when env.envLlvmOpts.flagLlvmInterpret $ printInterpretGrin (defs_bindingReturningCase ++ [def_drop])

  let (tagsToInt, llvm_ir) = mapAccumL grinToLlvm mempty (defs_bindingReturningCase ++ [def_drop])
      header =
        unlines
          [ "target triple = \"x86_64-unknown-linux-gnu\""
          , "declare void @printf(ptr, ...)"
          , "declare ptr @malloc(i64)"
          , "declare void @free(ptr)"
          -- FIXME data layout
          , "%Node = type [4 x i64]"
          ]
         ++ "@\"%d\" = private constant [4 x i8] c\"%d\\0A\\00\", align 1"

         -- debug
         ++ unlines
         [ "\n@\"HERE\" = private constant [9 x i8] c\"HERE %d\\0A\\00\", align  1"
         , "@\"sum\" = private constant [9 x i8] c\"sum: %d\\0A\\00\", align  1"
         , "@\"downFrom\" = private constant [14 x i8] c\"downFrom: %d\\0A\\00\", align 1"
         , "@\"_-_\" = private constant [9 x i8] c\"_-_: %d\\0A\\00\", align 1"
         , "@\"_+_\" = private constant [9 x i8] c\"_+_: %d\\0A\\00\", align 1"

         , "@\"rc: %d\" = private constant [9 x i8] c\"rc: %d\0A\00\", align 1"
         , "@\"tag: %d\" = private constant [10 x i8] c\"tag: %d\0A\00\", align 1"
         ]

      tags_table = "; Tag numbering table:\n" ++ prettyShow (align 20 (map (bimap ((++) "; " . show) pretty . swap) (sortOn snd (Map.toList tagsToInt))))
      render200 = renderStyle Style{ribbonsPerLine = 1.5, mode = PageMode, lineLength = 200}
      defs = intercalate "\n\n" (map (render200 . pretty) llvm_ir)
      program = intercalate "\n\n" [header, tags_table, defs]


  liftIO $ do
    let file_ll = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ ".ll"
    let file_ll_opt = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ "-opt.ll"
    let file_asm_s = "llvm" </> env.envLlvmOpts.flagLLvmOutput ++ "-asm.s"
    createDirectoryIfMissing False "llvm"
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

