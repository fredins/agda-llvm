
module DotProduct where

open import Data.Nat
open import Data.Vec
open import Function 

dotProduct : ∀ {n} → Vec ℕ n → Vec ℕ n → ℕ
dotProduct = sum ∘₂ zipWith _*_ 

main = dotProduct (1 ∷ 2 ∷ 3 ∷ []) (4 ∷ 5 ∷ 6 ∷ [])
