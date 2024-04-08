
open import Formalize.GlobalScope using (Globals)

module Formalize.MiniGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude renaming (mempty to ∅)
open import Haskell.Extra.Erase 

open import Scope 
open import Formalize.Utils.Erase 
open import Formalize.Syntax name globals

private variable
  @0 α β γ : Scope name

record Context : Set where
  constructor _¦_¦_
  field
    Ξ Δ Γ : Scope name 

infix 4 _¦_¦_ 

data SynthesizeVal 
 : Context 
 → Val α
 → Σ0[ β ∈ Scope name ] Rezz _ β × β ⊆ α 
 → Set

data SynthesizeTerm 
 : Context 
 → Term α
 → RcTerm α
 → Set

infix 3 SynthesizeVal SynthesizeTerm

syntax SynthesizeVal c v β = c ⊢ₛ v ⇝ᵥ β
syntax SynthesizeTerm c t t′ = c ⊢ₛ t ⇝ₜ t′

-- TODO add rest of the rules

data SynthesizeVal where
  SVAR-PRIM
    : ∀ {x}
    ---------------------------------------------------
    → (x ◃ ∅) ¦ ∅ ¦ ∅ ⊢ₛ Var x ⇝ᵥ < rezz ∅ , subEmpty >

-- TODO

-- perceusVal 
--   : (@0 Ξ Δ Γ : Scope name) (v : Val α) 
--   → Ξ ⋈ Δ ⋈ Γ ≡ α
--   → ∃ _ λ Σβ → Ξ ¦ Δ ¦ Γ ⊢ₛ v ⇝ᵥ Σβ
-- perceusVal v split = ?
