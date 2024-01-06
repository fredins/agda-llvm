
module DownFromVec where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

private
  variable
    A : Set
    m n : ℕ

infixr 5 _∷_

data Vec A : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ (x : A) (xs : Vec A n) → Vec A (suc n)

downFrom : (n : ℕ) → Vec ℕ n
downFrom zero = []
downFrom (suc n) = n ∷ downFrom n

sum : Vec ℕ n → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

main = sum (downFrom 100) -- 4950
