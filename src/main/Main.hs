module Main where

import           Agda.Llvm.Compiler
import           Agda.Main

main :: IO ()
main = runAgda [llvmBackend]
