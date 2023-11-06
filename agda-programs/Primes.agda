

module Primes where

open import Data.Nat.DivMod
open import Relation.Nullary.Decidable
open import Data.Nat
open import Data.Nat.Properties
open import Function
open import Data.List
open import Data.Bool

predicate : ℕ → ℕ → Bool
predicate n zero = true
predicate n (suc zero) = true
predicate n (suc (suc m)) = is-same ∨ not is-multiple
  where
  is-same = n ≡ᵇ suc (suc m)
  is-multiple = n % (suc (suc m)) ≡ᵇ 0

sieve : ℕ → List ℕ → List ℕ
sieve zero ns = ns
sieve (suc n) ns = sieve n $ boolFilter (flip predicate n) ns

primes : ℕ → List ℕ
primes n = sieve n $ upTo n

main = foldr _*_ 1 $ primes 20


  



