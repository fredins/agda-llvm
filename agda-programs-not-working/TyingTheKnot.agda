
module TyingTheKnot where

open import Agda.Builtin.Nat using (suc; zero; _+_; _-_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

{-# NON_TERMINATING #-}
cyclic₁ : ℕ → List ℕ
cyclic₂ : ℕ → List ℕ

cyclic₁ x = x ∷ cyclic₂ 1 
cyclic₂ x = x ∷ cyclic₁ 0

take : ℕ → List ℕ → List ℕ
take (suc n) (x ∷ xs) = x ∷ take n xs
take zero    xs        = []
take (suc n) []       = []

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

main = sum (take 100 (cyclic₁ 0)) - 2
