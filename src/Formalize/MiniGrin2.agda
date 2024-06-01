{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.MiniGrin2
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude 
open import Haskell.Prim.Tuple using (first; second; _***_)
open import Haskell.Extra.Refinement 
open import Haskell.Extra.Erase 
open import Haskell.Law.Equality 
open import Haskell.Law.Monoid.Def using (leftIdentity; rightIdentity)

open import Formalize.Scope2
open import Formalize.Utils.Erase 
open import Formalize.Syntax.Grin2 name globals
import Formalize.Syntax.RcGrin2 name globals as R

private variable
  @0 x       : name
  @0 α β γ δ : Scope name
  @0 Δ Γ     : Scope name

record Context : Set where
  constructor _¦_
  field
    borrowed owned : Scope name

infix 21 _¦_ 

intersect : 
  {@0 fvₗ fvᵣ : Scope name} {@0 Δ⊆ : Δ ⊆ α} {@0 Γ⊆ : Γ ⊆ α} {@0 fvₗ⊆ : fvₗ ⊆ α} {@0 fvᵣ⊆ : fvᵣ ⊆ α} → 
  Split Δ⊆ Γ⊆ → Cover fvₗ⊆ fvᵣ⊆ → Σ0[ Γᵣ ∈ Scope name ] Γᵣ ⊆ Γ × Γᵣ ⊆ fvᵣ
intersect empty       empty       = < empty , empty >
intersect (s left x)  (c left x)  = intersect s c
intersect (s left x)  (c right x) = mapSig (second (_discard x)) (intersect s c)
intersect (s left x)  (c both x)  = mapSig (second (_discard x)) (intersect s c)
intersect (s right x) (c left x)  = mapSig (first (_discard x)) (intersect s c)
intersect (s right x) (c right x) = mapSig ((_keep x) *** (_keep x)) (intersect s c)
intersect (s right x) (c both x)  = mapSig ((_keep x) *** (_keep x)) (intersect s c)

intersect₁ : 
  {@0 fvₗ fvᵣ : Scope name} {@0 Δ⊆ : Δ ⊆ α} {@0 Γ⊆ : Γ ⊆ α} {@0 fvₗ⊆ : fvₗ ⊆ α} {@0 fvᵣ⊆ : fvᵣ ⊆ α} → 
  Split Δ⊆ Γ⊆ → Cover fvₗ⊆ fvᵣ⊆ → Σ0[ Γᵣ ∈ Scope name ] Γᵣ ⊆ Γ 
intersect₁ s c = mapSig fst (intersect s c)


data SynthesizePair 
  {@0 α : Scope name} 
  {fₗ fᵣ fₗ′ fᵣ′ : @0 Scope name → Set} 
  (_⊢ₛ_⇝ₗ_ : {@0 α : Scope name} → Context → fₗ α → fₗ′ α → Set) 
  (_⊢ₛ_⇝ᵣ_ : {@0 α : Scope name} → Context → fᵣ α → fᵣ′ α → Set) : 
  Context → Pair fₗ fᵣ α → Pair fₗ′ fᵣ′ α → Set 


data SynthesizePair {α} {fₗ fᵣ fₗ′ fᵣ′} _⊢ₛ_⇝ₗ_ _⊢ₛ_⇝ᵣ_ where
  SPAIR : 
    {pair : Pair fₗ fᵣ α} {outl′ : fₗ′ _} {outr′ : fᵣ′ _} {@0 Δₗ Δᵣ Γₗ : Scope name} →
    {@0 Φ : Δ ⊆ α} {@0 Θ : Γ ⊆ α} (s : Split Φ Θ) →
    (let ⟨ Γᵣ ⟩ p = intersect₁ s (cover pair)) →
    Δₗ ¦ (diff p) ⊢ₛ outl pair ⇝ₗ outl′ →  
    Δᵣ ¦ Γᵣ       ⊢ₛ outr pair ⇝ᵣ outr′ →  
    ----------------------------
    SynthesizePair _ _ (Δ ¦ Γ) pair (MkPair (cover pair) outl′ outr′)

data SynthesizeBinder {@0 α β : Scope name} : 
  Context → Binder β Term α → Binder β R.Term α → Set

infix 3 _⊢ₛ_⇝ᵥ_ _⊢ₛ_⇝ₜ_ 

data _⊢ₛ_⇝ᵥ_ {@0 α : Scope name} : Context → Val α → R.Val α → Set where

data _⊢ₛ_⇝ₜ_ {@0 α : Scope name} : Context → Term α → R.Term α → Set where
  SBIND : 
    ∀ {r : Rezz (Scope name) β} {p p′} → 
    SynthesizePair _⊢ₛ_⇝ₜ_ SynthesizeBinder (Δ ¦ Γ) p p′ →
    ------------------------------------------------------
    Δ ¦ Γ ⊢ₛ Bind r p ⇝ₜ R.Bind r p′


data SynthesizeBinder {α β} where
  SBINDER :
    ∀ {binder body′} →
    Δ ¦ Γ ⊢ₛ body binder ⇝ₜ body′ → 
    --------------------------------------------------------------------------------
    SynthesizeBinder (Δ ¦ Γ) binder (MkBinder ⊆-refl (R.drops (usage binder) body′))
  
