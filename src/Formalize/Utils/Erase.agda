
module Formalize.Utils.Erase where

open import Haskell.Prelude using (_≡_; refl)
open import Haskell.Extra.Erase      using (Σ0; ⟨_⟩_; <_>; Erased; get; Erase) 
open import Haskell.Extra.Refinement using (∃; _⟨_⟩) 

infixr 2 ∃-syntax Σ0-syntax

∃-syntax  = ∃
{-# COMPILE AGDA2HS ∃-syntax inline #-}

Σ0-syntax = Σ0
{-# COMPILE AGDA2HS Σ0-syntax inline #-}

syntax ∃-syntax  a (λ x → b) =  ∃[ x ∈ a ] b
syntax Σ0-syntax a (λ x → b) = Σ0[ x ∈ a ] b

pattern ∃⟨_⟩  x = x ⟨ _ ⟩
pattern Σ0⟨_⟩ x = ⟨ _ ⟩ x

mapSig : {@0 a b : Set} {p : @0 a → Set} {q : @0 b → Set} {@0 f : a → b} → (∀ {@0 x} → p x → q (f x)) → Σ0 a p → Σ0 b q
mapSig g < s > = < g s >

{-# COMPILE AGDA2HS mapSig #-}

mapRef 
  : {a b : Set} {@0 p : a → Set} {@0 q : b → Set} {f : a → b} 
  → (∀ {@0 x} → p x → q (f x)) →  ∃ a p → ∃ b q
mapRef g (_ ⟨ x ⟩) = _ ⟨ g x ⟩


