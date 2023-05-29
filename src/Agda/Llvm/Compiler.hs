{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedRecordDot #-}


module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                (NFData)
import           GHC.Generics                   (Generic)

import           Agda.Compiler.Backend
import           Agda.Interaction.Options
import           Agda.Syntax.TopLevelModuleName
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
  , postCompile           = \_ _ _ -> pure undefined
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
data LlvmModule = LlvmModule
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

llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [LlvmDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ _ defs = error $ "Definitions: \n" ++ intercalate "\n" (map (show {- . theDef -}) defs)

llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM LlvmDefinition
llvmCompileDef env menv isMain def = pure $ LlvmDefinition def
