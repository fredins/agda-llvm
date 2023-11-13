
module DownFromTail where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

downFrom : List ℕ → ℕ → List ℕ
downFrom acc zero    = acc
downFrom acc (suc n) = downFrom (n ∷ acc) n

-- Your computer's memory is the limit!
main = sum 0 (downFrom [] 10_000_000)



