
module NFibSGhc where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)
open import Data.Nat.Show using (show)
open import Agda.Builtin.Strict using (primForce)

{-# TERMINATING #-}
nfib : ℕ → ℕ
nfib zero = 1
nfib (suc zero) = 1
nfib (suc (suc n)) = 
  primForce (primForce (suc n) nfib) _+_ 
    (primForce (primForce n nfib) _+_ 1)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main : IO ⊤
main = putStrLn (show (nfib 43))
