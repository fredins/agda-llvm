
module DownFrom where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = suc n ∷ downFrom n 

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

main = sum (downFrom 4)
