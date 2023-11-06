
module TestGhc where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)
open import Data.Nat.Show using (show)
open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

plusTwo : ℕ → ℕ
plusTwo = _+_ 2

main : IO ⊤
main = putStrLn (show (plusTwo 6))

