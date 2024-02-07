
module UpTo where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

upTo : List ℕ → ℕ → List ℕ
upTo acc zero = acc
upTo acc (suc n) = upTo (n ∷ acc) n

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (x + acc) xs

main = sum 0 (upTo [] 100) 

