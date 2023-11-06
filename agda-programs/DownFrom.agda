
module DownFrom where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = n ∷ downFrom n 

{-# STATIC downFrom #-}

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

{-# STATIC sum #-}

-- Current max: 58 000
main = sum (downFrom 100) 

