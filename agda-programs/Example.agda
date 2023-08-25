
module Example where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

private 
  variable
    A B : Set

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

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

main = sum (mapDouble (downFrom 100)) -- 9900
