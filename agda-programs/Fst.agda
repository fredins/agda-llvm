
module Fst where

open import Agda.Builtin.Nat renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

infixr 2 _×_
data _×_ A B : Set where  
 _,_ : A → B → A × B

fst : ℕ × ℕ → ℕ
fst (x , _) = x

{-# NOINLINE fst #-} 

nfib : ℕ → ℕ
nfib zero = 1
nfib (suc zero) = 1
nfib (suc (suc n)) = nfib (suc n) + nfib n + 1

main = nfib (primForce (10 , 100) fst)
