
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

infix 3 SynthesizeVal

syntax SynthesizeVal c v β = c ⊢ₛ v ⇝ᵥ β

-- TODO add rest of the rules
data SynthesizeVal where
  SVAR-PRIM
    : ∀ {x}
    ---------------------------------------------------
    → (x ◃ ∅) ¦ ∅ ¦ ∅ ⊢ₛ Var x ⇝ᵥ < rezz ∅ , subEmpty >

