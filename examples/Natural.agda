{-# OPTIONS --guardedness #-}

module Natural where

data ℕ : Set where
    zero : ℕ
    suc  : ℕ -> ℕ
-- {-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero + x₁  = x₁ 
suc x + x₁ = suc (x + x₁)

const : {A B : Set} →  A → B → A
const a b = a

double : ℕ → ℕ
double x = x + x
-- four = suc (suc (suc (suc zero)))
-- six = four + suc (suc zero)
