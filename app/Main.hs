module Main where

import           Agda.Main (runAgda)
import           Compiler.Llvm.Compiler (llvmBackend)

main :: IO ()
main = runAgda [llvmBackend]
