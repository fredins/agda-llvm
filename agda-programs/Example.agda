
module Example where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

private 
  variable
    A B : Set

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

infixr 5 _++_

_++_ : List ℕ → List ℕ → List ℕ
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

upTo : ℕ → List ℕ
upTo zero = zero ∷ []
upTo (suc n) = upTo n ++ suc n ∷ []

mapDouble : List ℕ → List ℕ
mapDouble []       = []
mapDouble (x ∷ xs) = x + x ∷ mapDouble xs

take : ℕ → List ℕ → List ℕ
take zero    _        = []
take (suc n) []       = []
take (suc n) (x ∷ xs) = x ∷ take n xs

sum : List ℕ → ℕ
sum []       = 0
sum (x ∷ xs) = x + sum xs

main = sum (take 10 (mapDouble (upTo 50)))


