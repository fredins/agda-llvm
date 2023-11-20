------------------------------------------------------------------------
-- A Tail recursive definition of the example program, which uses 
-- primForce to prevent space leaks.
------------------------------------------------------------------------

module DownFromTail where

open import Agda.Builtin.Nat using (suc; zero; _+_; _-_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A


downFrom : List ℕ → ℕ → List ℕ 
downFrom acc zero    = acc
downFrom acc (suc n) = downFrom (n ∷ acc) n

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

main = sum 0 (downFrom [] 100)



