{-# OPTIONS --guardedness #-}

module Io where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import IO using (run; _>>=_; _>>_; putStrLn; IO; Main)
open import Agda.Builtin.String using (String)
open import Data.Nat.Show using (show)
open import Function using (_$_)

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = n ∷ downFrom n 

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

main : Main
main = run do 
  putStrLn $ show $ sum $ downFrom 100
