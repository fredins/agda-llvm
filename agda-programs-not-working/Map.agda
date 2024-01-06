
module Map where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List : Set where
  []   : List 
  _∷_ : ℕ → List → List

map : (ℕ → ℕ) → List → List
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

main : List
main = map (_+_ 100) (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ [])
