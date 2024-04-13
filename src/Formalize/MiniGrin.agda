{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.MiniGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (_×_; _,_; Nat; _<>_; case_of_; []; _∷_; _$_; id) renaming (mempty to ∅)
open import Haskell.Prim.Tuple using (_***_; first; second)
open import Haskell.Extra.Erase 
open import Haskell.Extra.Refinement 
open import Haskell.Law

open import Scope 
open import Formalize.Utils.Erase 
open import Formalize.Scope
open import Formalize.Syntax.Grin name globals
import Formalize.Syntax.RcGrin name globals as Rc

private variable
  @0 x     : name
  @0 α β γ : Scope name

record Context : Set where
  constructor _¦_
  field
    Δ Γ : Scope name 

infix 4 _¦_ 

data SynthesizeName
 : Context 
 → (@0 x : name)
 → Rc.Name x
 → Set

data SynthesizeNames 
 : Context 
 → Names α
 → Rc.Names α
 → Set

data SynthesizeVal 
 : Context 
 → Val α
 → Rc.Val α
 → Set

data SynthesizeTerm 
 : Context 
 → Term α
 → Rc.Term α
 → Set

infix 3 SynthesizeName SynthesizeNames SynthesizeVal SynthesizeTerm 

syntax SynthesizeName c x x′    = c ⊢ₛ x  ⇝ₓ  x′
syntax SynthesizeNames c xs xs′ = c ⊢ₛ xs ⇝ₓₛ xs′
syntax SynthesizeVal c v v′     = c ⊢ₛ v  ⇝ᵥ  v′
syntax SynthesizeTerm c t t′    = c ⊢ₛ t  ⇝ₜ  t′

-- TODO add the rest of the rules

data SynthesizeName where

  SNAME-DUP
    : ∀ {x}
    -------------------------------------------
    → (x ◃ ∅) ¦ ∅ ⊢ₛ x ⇝ₓ Rc.Dup x

  SNAME
    : ∀ {x}
    ---------------------------------------------
    → ∅ ¦ (x ◃ ∅) ⊢ₛ x ⇝ₓ Rc.NoDup x

  -- TODO: add primitives later. 
  --
  -- SNAME-PRIM
  --   : ∀ {x}
  --   ----------------------------------------------
  --   → (x ◃ ∅) ¦ ∅ ¦ ∅ ⊢ₛ x ⇝ₓ Rc.NoDup x

data SynthesizeNames where
  SNIL
    ------------------------------
    : ∅ ¦ ∅ ⊢ₛ NNil ⇝ₓₛ Rc.NNil

  SCONS
    : ∀ {x x′ xs xs′ Δ Γ Δₗ Γₗ Δᵣ Γᵣ} {c : Cover (x ◃ ∅) β γ} 
    → Γₗ ⋈ Γᵣ ≡ Γ 
    → Δₗ ¦ Γₗ ⊢ₛ x  ⇝ₓ  x′ 
    → Δᵣ ¦ Γᵣ ⊢ₛ xs ⇝ₓₛ xs′
    ---------------------------------------------------------
    → Δ ¦ Γ ⊢ₛ NCons x c xs ⇝ₓₛ Rc.NCons x′ c xs′

data SynthesizeVal where
  SLIT
    : ∀ {n}
    ---------------------------------
    → ∅ ¦ ∅  ⊢ₛ Lit n ⇝ᵥ Rc.Lit n

  SVAR 
    : ∀ {Δ Γ x′}
    → Δ ¦ Γ  ⊢ₛ x ⇝ₓ x′
    ---------------------------------
    → Δ ¦ Γ  ⊢ₛ Var x ⇝ᵥ Rc.Var x′

data SynthesizeTerm where
  SRETURN
    : ∀ {Δ Γ v} {v′ : Rc.Val α}
    → Δ ¦ Γ ⊢ₛ v ⇝ᵥ v′
    ------------------------------------------------
    → Δ ¦ Γ ⊢ₛ Return v ⇝ₜ Rc.Return v′

  SAPPDEF 
    : ∀ {Δ Γ f p xs} {xs′ : Rc.Names α}
    → Δ ¦ Γ ⊢ₛ xs ⇝ₓₛ xs′
    ------------------------------------------------
    → Δ ¦ Γ ⊢ₛ AppDef f p xs ⇝ₜ Rc.AppDef f p xs′

-- Represents a split of a split Δ ⋈ Γ ≡ γ according to 
-- a Cover α β γ, such that the owned variables Γ are all consumed
-- exactly once. Thus, either α or β needs to borrow the shared 
-- variables. We do not enforce that α needs to borrow, unfortunately.
record CoverSplits (@0 α β Γ : Scope name) : Set where
  field
    @0 {Δₗ}    : Scope name
    @0 {Γₗ}    : Scope name
    @0 {Δᵣ}    : Scope name
    @0 {Γᵣ}    : Scope name
    splitLeft  : Δₗ ⋈ Γₗ ≡ α
    splitRight : Δᵣ ⋈ Γᵣ ≡ β
    splitGamma : Γₗ ⋈ Γᵣ ≡ Γ

{-# COMPILE AGDA2HS CoverSplits deriving Show #-}

open CoverSplits public

forCoverSplits 
 : ∀ {@0 α β Γ Δₗ′ Γₗ′ Δᵣ′ Γᵣ′ α′ β′ Γ′} 
 → (s : CoverSplits α β Γ)
 → (Δₗ s ⋈ Γₗ s ≡ α → Δₗ′ ⋈ Γₗ′ ≡ α′) 
 → (Δᵣ s ⋈ Γᵣ s ≡ β → Δᵣ′ ⋈ Γᵣ′ ≡ β′) 
 → (Γₗ s ⋈ Γᵣ s ≡ Γ → Γₗ′ ⋈ Γᵣ′ ≡ Γ′)
 → CoverSplits α′ β′ Γ′
forCoverSplits s f g h = record 
  { splitLeft  = f (splitLeft s)
  ; splitRight = g (splitRight s)
  ; splitGamma = h (splitGamma s)
  }

{-# COMPILE AGDA2HS forCoverSplits  #-}

opaque 
  unfolding Split
  -- Create a CoverSplits. If a varaible is used by both α and β then it is borrowed in α and owned in β.
  coverSplits : ∀{@0 Δ Γ} → Cover α β γ → Δ ⋈ Γ ≡ γ → CoverSplits α β Γ
  coverSplits CDone      EmptyL      = record{splitLeft = splitEmptyLeft; splitRight = splitEmptyLeft; splitGamma = splitEmptyLeft}
  coverSplits CDone      EmptyR      = record{splitLeft = splitEmptyRight; splitRight = splitEmptyRight; splitGamma = splitEmptyRight}
  coverSplits (CLeft  c) EmptyL      = forCoverSplits (coverSplits c EmptyL) splitBindRight id             splitBindLeft
  coverSplits (CLeft  c) EmptyR      = forCoverSplits (coverSplits c EmptyR) splitBindLeft  id             id
  coverSplits (CLeft  c) (ConsL x s) = forCoverSplits (coverSplits c s)      splitBindLeft  id             id
  coverSplits (CLeft  c) (ConsR x s) = forCoverSplits (coverSplits c s)      splitBindRight id             splitBindLeft
  coverSplits (CRight c) EmptyL      = forCoverSplits (coverSplits c EmptyL) id             splitBindRight splitBindRight
  coverSplits (CRight c) EmptyR      = forCoverSplits (coverSplits c EmptyR) id             splitBindLeft  id
  coverSplits (CRight c) (ConsL x s) = forCoverSplits (coverSplits c s)      id             splitBindLeft  id
  coverSplits (CRight c) (ConsR x s) = forCoverSplits (coverSplits c s)      id             splitBindRight splitBindRight
  coverSplits (CBoth  c) EmptyL      = forCoverSplits (coverSplits c EmptyL) splitBindLeft  splitBindRight splitBindRight
  coverSplits (CBoth  c) EmptyR      = forCoverSplits (coverSplits c EmptyR) splitBindLeft  splitBindLeft  id
  coverSplits (CBoth  c) (ConsL x s) = forCoverSplits (coverSplits c s)      splitBindLeft  splitBindLeft  id
  coverSplits (CBoth  c) (ConsR x s) = forCoverSplits (coverSplits c s)      splitBindLeft  splitBindRight splitBindRight

  {-# COMPILE AGDA2HS coverSplits  #-}

  perceusName 
    : ∀ {@0 Δ Γ} 
    → Δ ⋈ Γ ≡ (x ◃ ∅)
    → ∃[ x′ ∈ Rc.Name x ] Δ ¦ Γ ⊢ₛ x ⇝ₓ x′
  perceusName EmptyL           = _ ⟨ SNAME ⟩
  perceusName EmptyR           = _ ⟨ SNAME-DUP ⟩
  perceusName (ConsL x EmptyL) = _ ⟨ SNAME-DUP ⟩
  perceusName (ConsL x EmptyR) = _ ⟨ SNAME-DUP ⟩
  perceusName (ConsR x EmptyL) = _ ⟨ SNAME ⟩
  perceusName (ConsR x EmptyR) = _ ⟨ SNAME ⟩


  {-# COMPILE AGDA2HS perceusName  #-}

  perceusNames
    : ∀ {@0 Δ Γ} (xs : Names α)
    → Δ ⋈ Γ ≡ α
    → ∃[ xs′ ∈ Rc.Names α ] Δ ¦ Γ ⊢ₛ xs ⇝ₓₛ xs′
  perceusNames NNil EmptyL = _ ⟨ SNIL ⟩
  perceusNames NNil EmptyR = _ ⟨ SNIL ⟩
  perceusNames (NCons x c xs) s = 
    let 
      cs = coverSplits c s 
      _ ⟨ proof₁ ⟩ = perceusName (CoverSplits.splitLeft cs)
      _ ⟨ proof₂ ⟩ = perceusNames xs (CoverSplits.splitRight cs)
    in
    _ ⟨ SCONS (CoverSplits.splitGamma cs) proof₁ proof₂ ⟩


  {-# COMPILE AGDA2HS perceusNames  #-}

  perceusLit 
    : ∀ {@0 Δ Γ} {n : Nat}
    → Δ ⋈ Γ ≡ ∅
    → Δ ¦ Γ ⊢ₛ Lit n ⇝ᵥ Rc.Lit n
  perceusLit EmptyL = SLIT
  perceusLit EmptyR = SLIT

perceusVal 
  : ∀ {@0 Δ Γ} (v : Val α) 
  → Δ ⋈ Γ ≡ α
  → ∃[ v′ ∈ Rc.Val α ] Δ ¦ Γ ⊢ₛ v ⇝ᵥ v′
perceusVal (Lit n) split = _ ⟨ perceusLit split ⟩
perceusVal (Var x) split = 
    let _ ⟨ proof ⟩ = perceusName split in 
    _ ⟨ SVAR proof ⟩ 

{-# COMPILE AGDA2HS perceusVal  #-}

perceusTerm 
  : ∀ {@0 Δ Γ} (t : Term α) 
  → Δ ⋈ Γ ≡ α
  → ∃[ t′ ∈ Rc.Term α ] Δ ¦ Γ ⊢ₛ t ⇝ₜ t′
perceusTerm (Return v) split = 
  let _ ⟨ proof ⟩ = perceusVal v split in
  _ ⟨ SRETURN proof ⟩
perceusTerm (AppDef f p xs) split = 
  let _ ⟨ proof ⟩ = perceusNames xs split in
  _ ⟨ SAPPDEF proof ⟩


{-# COMPILE AGDA2HS perceusTerm  #-}
