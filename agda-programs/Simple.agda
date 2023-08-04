
module Simple where

data ℕ : Set where
    zero : ℕ
    suc  : ℕ -> ℕ
{-# BUILTIN NATURAL ℕ #-}

four : ℕ
four = 4
