
module Goal where

data ℕ : Set where
    zero : ℕ
    suc  : ℕ -> ℕ
{-# BUILTIN NATURAL ℕ #-}


infixr 5 _∷_
data List : Set where
  []   : List 
  _∷_ : ℕ → List → List

map : (ℕ → ℕ) → List → List
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_+_ : ℕ → ℕ → ℕ
zero + x₁  = x₁ 
suc x + x₁ = suc (x + x₁)
{-# BUILTIN NATPLUS _+_ #-}

main : List
main = map (_+_ 100) (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ [])
