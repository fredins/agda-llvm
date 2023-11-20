{-# OPTIONS --guardedness #-}

module Vectorize where

open import Function
open import IO 
open import Data.Maybe using (maybe)
open import Data.Nat.Show using (show; readMaybe)
open import Data.Unit.Polymorphic using (tt)
open import Data.List 
open import Data.Nat



list : List ℕ
list = upTo 10

vectorize-me : ℕ → List ℕ
vectorize-me n = map (_+ n) list

main = run do 
  s ← getLine
  maybe 
    (putStrLn ∘ show ∘ sum ∘ vectorize-me)
    (pure tt) 
    (readMaybe 10 s)


