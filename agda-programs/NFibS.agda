
module NFibS where

open import Agda.Builtin.Nat using (suc; zero; _+_; _-_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

{-# TERMINATING #-}
nfib : ℕ → ℕ
nfib zero = 1
nfib (suc zero) = 1
nfib (suc (suc n)) = 
  primForce (primForce (suc n) nfib) _+_ 
    (primForce (primForce n nfib) _+_ 1)

main = nfib 43
