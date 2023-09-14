
module DownFromTail where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

private 
  variable
    A B : Set

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : List ℕ → ℕ → List ℕ
downFrom acc zero    = acc
downFrom acc (suc n) = downFrom (n ∷ acc) n

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (x + acc) xs

main = sum 0 (downFrom [] 74_000) -- 100 → 4950
