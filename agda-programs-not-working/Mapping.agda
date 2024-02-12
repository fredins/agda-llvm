
module Mapping where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = n ∷ downFrom n

mapDouble : List ℕ → List ℕ
mapDouble []       = []
mapDouble (x ∷ xs) = x + x ∷ mapDouble xs

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

main = sum 0 (mapDouble (downFrom 10))
