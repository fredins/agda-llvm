{-# OPTIONS --guardedness #-}

module natural where

data ℕ : Set where
    zero : ℕ
    suc  : ℕ -> ℕ

{-# BUILTIN NATURAL ℕ #-}

two = suc (suc zero)


