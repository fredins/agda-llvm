{- TODO
   • Use internal syntax instead of treeless 
   • Figure out how to determine argument
   • Algorithm to split multiple applications into lets
   • Use de bruijn for Grin
   • New syntax?
-}

module Sum where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ)

infixr 5 _∷_
data List : Set where
  []   : List 
  _∷_  : ℕ → List → List

sum : List → ℕ
sum [] = 0
sum (x ∷ xs) = suc (sum xs)

main = let xs = 1 ∷ 2 ∷ 3 ∷ [] in sum xs
