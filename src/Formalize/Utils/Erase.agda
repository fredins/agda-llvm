
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

@0 inverse-erased-get : {@0 a : Set} {@0 z : a} {x : Erase a} → Erased z ≡ x → z ≡ get x
inverse-erased-get refl = refl

@0 inverse-get-erased : {@0 a : Set} {@0 z : a} {x : Erase a} → z ≡ get x → Erased z ≡ x
inverse-get-erased refl = refl
 
map0 : {@0 a b : Set} {p : @0 a → Set} {q : @0 b → Set} {@0 f : a → b} → (∀ {@0 x} → p x → q (f x)) → Σ0 a p → Σ0 b q
map0 g < s > = < g s >
