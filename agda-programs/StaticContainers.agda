
module StaticContainers where

open import Data.Nat
open import Data.Product
open import Agda.Builtin.Equality
open import Relation.Nullary.Decidable
open import Data.Fin using (Fin; zero; suc; fromℕ)

module Array where
  Array : Set → (n : ℕ) {≢0 : False (n ≟ 0)} → Set
  Array A (suc zero) = A
  Array A (suc (suc n)) = A × Array A (suc n)

  4-array : Array ℕ 4
  4-array = 1 , 2 , 3 , 4

  proj₃ : {A : Set} → Array A 4 → A
  proj₃ (x₁ , x₂ , x₃ , x₄) = x₃

  _ : proj₃ 4-array ≡ 3
  _ = refl

  lookup : ∀ {n} {≢0 : False (n ≟ 0)} {A : Set} → Fin n → Array A n {≢0} → A
  lookup {suc zero} zero x = x
  lookup {suc (suc n)} zero (x , _) = x
  lookup {suc (suc n)} (suc i) (_ , xs) = lookup {suc n} i xs

  _ : lookup zero (1 , 2 , 3 , 4) ≡ 1
  _ = refl

  _ : lookup (suc zero) (1 , 2 , 3 , 4) ≡ 2
  _ = refl

  _ : lookup (suc (suc (zero))) (1 , 2 , 3 , 4) ≡ 3
  _ = refl

  _ : lookup (suc (suc (suc (zero)))) (1 , 2 , 3 , 4) ≡ 4
  _ = refl


module Matrix where
  open Array

  Matrix : Set → (rows cols : ℕ) → {False (rows ≟ 0)} → {False (cols ≟ 0)} → Set
  Matrix A rows cols {rows≢0} {cols≢0} = Array (Array A cols {cols≢0}) rows {rows≢0}

  4×2-matrix : Matrix ℕ 4 2
  4×2-matrix = (1 , 2) , (3 , 4) , (5 , 6) , (7 , 8) 

  _ : proj₁ (proj₁ (proj₂ (proj₂ 4×2-matrix))) ≡ 5
  _ = refl 




-- DynamicArray
-- DynamicArray : 
--   (length : ℕ) 
--   {capacity : ℕ} 
--   (A : Set)
--   {length≢0 : False (length ≟ 0)} 
--   {length<capacity : True (length <? capacity)} → 
--   Set
-- DynamicArray length {capacity} A = ?
-- create a pair record and use the Zero type for indexes that exceed the length
    

