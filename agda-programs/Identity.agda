
module Identity where

open import Agda.Builtin.Nat renaming (Nat to ℕ) 

const : ℕ → ℕ → ℕ
const x y = x

main = const 10 5
