{-# OPTIONS --guardedness #-}

module DownFromIO where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import IO using (run; _>>=_; _>>_; putStrLn; IO; Main; getLine; pure)
open import Agda.Builtin.String using (String)
open import Data.Nat.Show using (show; readMaybe)
open import Data.Unit.Polymorphic using (tt)
open import Function using (_$_; _∘_; case_of_)
open import Data.Maybe using (just; nothing; maybe)

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
  s ← getLine
  maybe 
    (putStrLn ∘ show ∘ sum ∘ downFrom) 
    (pure tt) 
    (readMaybe 10 s)
    
