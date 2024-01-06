
module DownFromOptGhc where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)
open import Data.Nat.Show using (show)
open import Agda.Builtin.Strict using (primForce)

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

{-# TERMINATING #-}
downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = primForce n λ n → n ∷ downFrom n

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main : IO ⊤
main = putStrLn (show (sum 0 (downFrom 100_000_000_000)))
