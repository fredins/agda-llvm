{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                (NFData)
import           GHC.Generics                   (Generic)

import           Agda.Compiler.Backend
import           Agda.Interaction.Options
import           Agda.Syntax.TopLevelModuleName
import           Agda.Utils.Functor             (for)
import           Agda.Utils.Impossible          (__IMPOSSIBLE__)
import           Agda.Utils.Maybe               (caseMaybeM)
import           Agda.Utils.Pretty
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Debug.Trace                    (trace)
import           GHC.OldList                    (intercalate)

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LlvmOptions LlvmEnv LlvmModuleEnv LlvmModule LlvmDefinition
llvmBackend' = Backend'
  { backendName           = "LLVM"
  , backendVersion        = Nothing
  , options               = defaultLlvmOptions
  , commandLineFlags      = llvmCommandLineFlags
  , isEnabled             = flagLlvmCompile
  , preCompile            = const $ pure LlvmEnv
  , postCompile           = llvmPostCompile
  , preModule             = \_ _ _ -> pure $ pure $ Recompile LlvmModuleEnv
  , postModule            = llvmPostModule
  , compileDef            = llvmCompileDef
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ pure True
  }

data LlvmOptions = LlvmOptions
  { flagLlvmCompile :: Bool
  } deriving (Generic, NFData)


data LlvmEnv = LlvmEnv
data LlvmModuleEnv = LlvmModuleEnv
data LlvmModule = LlvmModule [LlvmDefinition]
data LlvmDefinition = LlvmDefinition Definition deriving Show


defaultLlvmOptions :: LlvmOptions
defaultLlvmOptions = LlvmOptions
  { flagLlvmCompile = False
  }


llvmCommandLineFlags :: [OptDescr (Flag LlvmOptions)]
llvmCommandLineFlags =
    [ Option ['c']  ["compile", "llvm"] (NoArg enable)
                    "compile program using the LLvm backend"
    ]
  where
    enable o = pure o{ flagLlvmCompile = True }

llvmPostCompile :: LlvmEnv
                -> IsMain
                -> Map TopLevelModuleName LlvmModule
                -> TCM ()
llvmPostCompile _ _ mods = error str where
  str = intercalate "\n"
      $ for (Map.elems mods)
      $ \(LlvmModule defs) -> intercalate "\n "
      $ for defs
      $ \(LlvmDefinition def) -> show def


llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [LlvmDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ _ defs = pure $ LlvmModule defs

llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM LlvmDefinition
llvmCompileDef env menv isMain def@Defn{defName=q, theDef=Function{}} = do
  _ <- toTreeless LazyEvaluation q
  def' <- getConstInfo q
  pure $ LlvmDefinition def'
llvmCompileDef env menv isMain def = pure $ LlvmDefinition def



-- class Pretty' a where
--   pretty' :: a -> Doc


-- instance Pretty' Definition where
--   pretty' Defn{..} =
--     "Defn {" <?> vcat
--       [ "defArgInfo        =" <?> pshow defArgInfo
--       , "defName           =" <?> pretty defName
--       , "defType           =" <?> pretty defType
--       , "defPolarity       =" <?> pshow defPolarity
--       , "defArgOccurrences =" <?> pshow defArgOccurrences
--       , "defGeneralizedParams =" <?> pshow defGeneralizedParams
--       , "defDisplay        =" <?> pretty defDisplay
--       , "defMutual         =" <?> pshow defMutual
--       , "defCompiledRep    =" <?> pshow defCompiledRep
--       , "defInstance       =" <?> pshow defInstance
--       , "defCopy           =" <?> pshow defCopy
--       , "defMatchable      =" <?> pshow (Set.toList defMatchable)
--       , "defInjective      =" <?> pshow defInjective
--       , "defCopatternLHS   =" <?> pshow defCopatternLHS
--       , "theDef            =" <?> pretty' theDef ] <+> "}"
--
--
-- instance Pretty' Defn where
--   pretty' = \case
--     AxiomDefn _         -> "Axiom"
--     DataOrRecSigDefn d  -> pretty d
--     GeneralizableVar    -> "GeneralizableVar"
--     AbstractDefn def    -> "AbstractDefn" <?> parens (pretty def)
--     FunctionDefn d      -> pretty' d
--     DatatypeDefn d      -> pretty d
--     RecordDefn d        -> pretty d
--     ConstructorDefn d   -> pretty d
--     PrimitiveDefn d     -> pretty d
--     PrimitiveSortDefn d -> pretty d
--
-- instance Pretty' FunctionData where
--   pretty' (FunctionData
--       funClauses
--       funCompiled
--       funSplitTree
--       funTreeless
--       _funCovering
--       funInv
--       funMutual
--       funAbstr
--       funDelayed
--       funProjection
--       funFlags
--       funTerminates
--       _funExtLam
--       funWith
--       funIsKanOp
--     ) =
--     "Function {" <?> vcat
--       [ "funClauses      =" <?> vcat (map pretty funClauses)
--       , "funCompiled     =" <?> pretty funCompiled
--       , "funSplitTree    =" <?> pretty funSplitTree
--       , "funTreeless     =" <?> pretty funTreeless
--       , "funInv          =" <?> pretty funInv
--       , "funMutual       =" <?> pshow funMutual
--       , "funAbstr        =" <?> pshow funAbstr
--       , "funDelayed      =" <?> pshow funDelayed
--       , "funProjection   =" <?> pretty funProjection
--       , "funFlags        =" <?> pshow funFlags
--       , "funTerminates   =" <?> pshow funTerminates
--       , "funWith         =" <?> pretty funWith
--       , "funIsKanOp      =" <?> pretty funIsKanOp
--       ] <?> "}"
--
-- instance Pretty Compiled where
--   pretty Compiled {cTreeless, cArgUsage} =
--     "Compiled {" <?> vcat
--       [ "cTreeless   =" <?> pretty cTreeless
--       , "funCompiled =" <?> pshow cArgUsage
--       ] <?> "}"
