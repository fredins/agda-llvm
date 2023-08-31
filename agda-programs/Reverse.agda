
module Reverse where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

private 
  variable
    A B : Set

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A


reverse : List ℕ → List ℕ → List ℕ
reverse (x ∷ xs) acc = reverse xs (x ∷ acc)
reverse []       acc = acc


