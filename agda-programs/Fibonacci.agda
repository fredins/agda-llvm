-- {-# OPTIONS --sized-types #-}
{-# OPTIONS --sized-types --guardedness #-}

module Fibonacci where

open import Agda.Builtin.Nat using (suc; zero; _+_; _-_) renaming (Nat to ℕ) 

-- Naive fibonacci
-- fib : ℕ → ℕ
-- fib zero = 1
-- fib (suc zero) = 1
-- fib (suc (suc n)) = fib (suc n) + fib n

open import Codata.Guarded.Stream

fibs′ : ℕ → ℕ → Stream ℕ
head (fibs′ x y) = x
tail (fibs′ x y) = fibs′ y (x + y)

fibs : Stream ℕ
fibs = fibs′ 0 1

main : ℕ
main = lookup fibs 100

