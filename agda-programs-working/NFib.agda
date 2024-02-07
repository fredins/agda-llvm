
module NFib where

open import Agda.Builtin.Nat using (suc; zero; _+_; _-_) renaming (Nat to ℕ) 

nfib : ℕ → ℕ
nfib zero = 1
nfib (suc zero) = 1
nfib (suc (suc n)) = nfib (suc n) + nfib n + 1

main = nfib 15
