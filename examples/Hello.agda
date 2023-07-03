
module Hello where

open import Agda.Builtin.IO
open import Agda.Builtin.String
open import Agda.Builtin.Unit using (⊤)

-- open import Data.Tree.AVL using () -- unused import

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as Text #-}
{-# COMPILE GHC putStrLn = putStrLn . Text.unpack #-}

main : IO ⊤
main = putStrLn "Hello world!"
