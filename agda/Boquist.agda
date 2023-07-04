module Boquist where

open import Agda.Builtin.Nat using (suc; zero; _+_; _==_; _<_) renaming (Nat to ℕ)
open import Agda.Builtin.Bool using (false; true)
open import Agda.Builtin.Equality using (_≡_)
open import Data.Nat.Base using (_≤_; _≤‴_; ≤‴-refl; ≤‴-step)
open import Data.Nat.Properties using (m≤‴m+k; +-identity; +-suc) 

infixr 5 _∷_
data List : Set where
  []   : List 
  _∷_  : ℕ → List → List

infixr 5 _++_

_++_ : List → List → List
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

[_] : ℕ → List
[ n ] = n ∷ []

infixl 6 _∷ʳ_

_∷ʳ_ : List → ℕ → List
xs ∷ʳ x = xs ++ [ x ]

upto : (m n : ℕ) → {m ≤‴ n} → List
upto m n {≤‴-step p} = m ∷ upto (suc m) n {p}
upto m _ {≤‴-refl} = [ m ]

sum : ℕ → List → ℕ
sum n [] = n
sum n (x ∷ xs) = sum (n + x) xs

4≤‴100-proof : 4 ≤‴ 100
4≤‴100-proof = m≤‴m+k (+-suc 4 95)

main = sum 0 (upto 4 100 {4≤‴100-proof})





