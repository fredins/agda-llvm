

module Primes where

open import Agda.Builtin.Nat using (suc; zero; mod-helper) renaming (Nat to ℕ) 

private 
  variable
    m n : ℕ



noll : ℕ
noll = zero


-- _%_ : suc m → suc n → ℕ
-- m % n = ?
