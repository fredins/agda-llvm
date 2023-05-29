module Main where

import           Agda.Llvm.Compiler (llvmBackend)
import           Agda.Main          (runAgda)

main :: IO ()
main = runAgda [llvmBackend]
