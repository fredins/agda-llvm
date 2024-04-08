
open import Formalize.GlobalScope using (Globals)

module Formalize.MiniGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (_×_; _,_; Nat; case_of_) renaming (mempty to ∅)
open import Haskell.Extra.Erase 
open import Haskell.Extra.Refinement 

open import Scope 
open import Formalize.Utils.Erase 
open import Formalize.Scope
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
  SLIT
    : ∀ {n}
    --------------------------------------
    → ∅ ¦ ∅ ¦ ∅  ⊢ₛ Lit n ⇝ᵥ < rezz ∅ , subEmpty >

  SVAR-PRIM
    : ∀ {x}
    ---------------------------------------------------
    → (x ◃ ∅) ¦ ∅ ¦ ∅ ⊢ₛ Var x ⇝ᵥ < rezz ∅ , subEmpty >

  SVAR-DUP
    : ∀ {x}
    --------------------------------------------------------
    → ∅ ¦ (x ◃ ∅) ¦ ∅ ⊢ₛ Var x ⇝ᵥ < rezz (x ◃ ∅) , subRefl >

  SVAR
    : ∀ {x}
    ---------------------------------------------------
    → ∅ ¦ ∅ ¦ (x ◃ ∅) ⊢ₛ Var x ⇝ᵥ < rezz ∅ , subEmpty >


data SynthesizeTerm where


opaque 
  unfolding Split3 Split
  perceusLit 
    : (@0 Ξ Δ Γ : Scope name) (n : Nat) 
    → Ξ ⋈ Δ ⋈ Γ ≡ ∅
    → Ξ ¦ Δ ¦ Γ ⊢ₛ Lit n ⇝ᵥ < rezz ∅ , subEmpty >
  perceusLit Ξ Δ Γ n < EmptyL , EmptyL > = SLIT
  perceusLit Ξ Δ Γ n < EmptyL , EmptyR > = SLIT
  perceusLit Ξ Δ Γ n < EmptyR , EmptyL > = SLIT
  perceusLit Ξ Δ Γ n < EmptyR , EmptyR > = SLIT

opaque 
  unfolding Split3 Split Sub
  perceusVar 
    : (@0 Ξ Δ Γ : Scope name) (@0 x : name) 
    → Ξ ⋈ Δ ⋈ Γ ≡ (x ◃ ∅)
    → ∃[ β ∈ _ ] Ξ ¦ Δ ¦ Γ ⊢ₛ Var x ⇝ᵥ β
  perceusVar Ξ Δ Γ x < EmptyL          , EmptyL          > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < EmptyL          , EmptyR          > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < EmptyL          , ConsL .x EmptyL > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < EmptyL          , ConsL .x EmptyR > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < EmptyL          , ConsR .x EmptyL > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < EmptyL          , ConsR .x EmptyR > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < EmptyR          , EmptyL          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < EmptyR          , EmptyR          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < ConsL .x EmptyL , EmptyL          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < ConsL .x EmptyR , EmptyL          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < ConsL .x EmptyL , EmptyR          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < ConsL .x EmptyR , EmptyR          > = _ ⟨ SVAR-PRIM ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , EmptyL          > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , EmptyR          > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , ConsL .x EmptyL > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , ConsL .x EmptyR > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , ConsR .x EmptyL > = _ ⟨ SVAR ⟩ 
  perceusVar Ξ Δ Γ x < ConsR .x EmptyL , ConsR .x EmptyR > = _ ⟨ SVAR ⟩ 
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , EmptyL          > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , EmptyR          > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , ConsL .x EmptyL > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , ConsL .x EmptyR > = _ ⟨ SVAR-DUP ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , ConsR .x EmptyL > = _ ⟨ SVAR ⟩
  perceusVar Ξ Δ Γ x < ConsR .x EmptyR , ConsR .x EmptyR > = _ ⟨ SVAR ⟩

perceusVal 
  : (@0 Ξ Δ Γ : Scope name) (v : Val α) 
  → Ξ ⋈ Δ ⋈ Γ ≡ α
  → ∃[ β ∈ _ ] Ξ ¦ Δ ¦ Γ ⊢ₛ v ⇝ᵥ β
perceusVal Ξ Δ Γ (Lit n) split = _ ⟨ perceusLit Ξ Δ Γ n split ⟩
perceusVal Ξ Δ Γ (Var x) split = perceusVar Ξ Δ Γ x split
