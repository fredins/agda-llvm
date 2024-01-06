
module Identity where

open import Agda.Builtin.Nat renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

id : ℕ → ℕ
id x = x

{-# NOINLINE id #-} 

nfib : ℕ → ℕ
nfib zero = 1
nfib (suc zero) = 1
nfib (suc (suc n)) = nfib (suc n) + nfib n + 1

main = nfib (primForce 10 id)
