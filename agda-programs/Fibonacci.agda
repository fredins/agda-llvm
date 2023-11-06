
module Fibonacci where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

fib : ℕ → ℕ
fib zero = 1
fib (suc zero) = 1
fib (suc (suc n)) = fib (suc n) + fib n


fibs : List ℕ → ℕ → List ℕ
fibs acc zero = 1 ∷ []
fibs acc (suc n) = 1 ∷ 1 ∷ [] 
fibs acc (suc (suc n)) = fibs 

main = fib 100
