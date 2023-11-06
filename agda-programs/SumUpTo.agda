
module SumUpTo where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

sumUpTo : ℕ → ℕ → ℕ
sumUpTo acc (suc n) = sumUpTo (n + acc) n
sumUpTo acc zero = acc

main = sumUpTo 0 50_000
