{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.MiniGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude -- using (_<>_; _,_; Show) 
open import Haskell.Prim.Tuple using (first; _***_)
open import Haskell.Extra.Refinement 
open import Haskell.Extra.Erase 
open import Haskell.Law.Equality 
open import Haskell.Law.Monoid.Def using (leftIdentity; rightIdentity)

open import Formalize.Scope
open import Formalize.Utils.Erase 
open import Formalize.Syntax.Grin name globals
import Formalize.Syntax.RcGrin name globals as R

private variable
  @0 x       : name
  @0 α β γ δ : Scope name
  @0 Δ Γ     : Scope name

record Context : Set where
  constructor _¦_
  field
    borrowed owned : Scope name

infix 4 _¦_ 

-- Represents a split of a split Δ ⋈ Γ ≡ γ according to 
-- a Cover α β γ, such that the owned variables Γ are all consumed
-- exactly once. We enforce that that the left term (α) must borrow 
-- the shared variables.
record SplitCover (@0 Δ Γ α β  : Scope name) : Set where
  field
    -- The subsripts l, r, and lr indicate subscopeness of 
    -- only α, only β, or both α and β.
    @0 {Δₗᵣ Δₗ Δᵣ}  : Scope name
    @0 {Γₗᵣ Γₗ Γᵣ}  : Scope name

    -- Composite scopes. Note funny names.
    @0 {Δₗᵣ,Δₗ,Γₗᵣ} : Scope name
    @0 {Δₗᵣ,Δᵣ}     : Scope name
    @0 {Γₗᵣ,Γᵣ}     : Scope name

    -- Auxiliary splits.
    splitlDelta     : Δₗᵣ ⋈ Δₗ ⋈ Γₗᵣ ≡ Δₗᵣ,Δₗ,Γₗᵣ
    splitrDelta     : Δₗᵣ ⋈ Δᵣ       ≡ Δₗᵣ,Δᵣ
    splitrGamma     : Γₗᵣ ⋈ Γᵣ       ≡ Γₗᵣ,Γᵣ

    splitDelta      : Δₗᵣ ⋈ Δₗ ⋈ Δᵣ ≡ Δ
    splitGamma      : Γₗᵣ ⋈ Γₗ ⋈ Γᵣ ≡ Γ

    splitl          : Δₗᵣ,Δₗ,Γₗᵣ ⋈ Γₗ     ≡ α
    splitr          : Δₗᵣ,Δᵣ     ⋈ Γₗᵣ,Γᵣ ≡ β

{-# COMPILE AGDA2HS SplitCover deriving Show  #-}

open SplitCover public

-- Create a SplitCover. The function body is huge but the details are
-- irrelevant as it is correct by construction.
splitAndCover : Δ ⋈ Γ ≡ γ → Cover α β γ → SplitCover Δ Γ α β
splitAndCover SEmptyL CEmptyL = 
  record { splitlDelta = < SEmptyR , SEmptyR >
         ; splitrDelta = SEmptyR
         ; splitrGamma = SEmptyL
         ; splitDelta  = < SEmptyR , SEmptyR >
         ; splitGamma  = < SEmptyL , SEmptyL >
         ; splitl      = SEmptyR
         ; splitr      = SEmptyL
         }
splitAndCover SEmptyL CEmptyR = 
  record { splitlDelta = < SEmptyR , SEmptyR >
         ; splitrDelta = SEmptyR
         ; splitrGamma = SEmptyR
         ; splitDelta  = < SEmptyR , SEmptyR >
         ; splitGamma  = < SEmptyL , SEmptyR >
         ; splitl      = SEmptyL
         ; splitr      = SEmptyR
         }
splitAndCover SEmptyL (CExtendL c x) = 
  let sc = splitAndCover SEmptyL c in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = splitDelta sc 
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitGamma sc) 
         ; splitl      = SExtendR (splitl sc) x 
         ; splitr      = splitr sc 
         }
splitAndCover SEmptyL (CExtendR c x) = 
  let sc = splitAndCover SEmptyL c in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = SExtendR (splitrGamma sc) x
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitGamma sc)
         ; splitl      = splitl sc
         ; splitr      = SExtendR (splitr sc) x
         }
splitAndCover SEmptyL (CExtendB c x) = 
  let sc = splitAndCover SEmptyL c in 
  record { splitlDelta = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitlDelta sc)
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = SExtendL (splitrGamma sc) x 
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig (first λ s → SExtendL s x) (splitGamma sc)
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = SExtendR (splitr sc) x
         }
splitAndCover SEmptyR CEmptyL = 
  record { splitlDelta = < SEmptyL , SEmptyL >
         ; splitrDelta = SEmptyL
         ; splitrGamma = SEmptyL
         ; splitDelta  = < SEmptyL , SEmptyL >
         ; splitGamma  = < SEmptyR , SEmptyL >
         ; splitl      = SEmptyR
         ; splitr      = SEmptyR
         }
splitAndCover SEmptyR CEmptyR = 
  record { splitlDelta = < SEmptyL , SEmptyR >
         ; splitrDelta = SEmptyL
         ; splitrGamma = SEmptyL
         ; splitDelta  = < SEmptyL , SEmptyR >
         ; splitGamma  = < SEmptyL , SEmptyL >
         ; splitl      = SEmptyR
         ; splitr      = SEmptyR
         }
splitAndCover SEmptyR (CExtendL c x) = 
  let sc = splitAndCover SEmptyR c in 
  record { splitlDelta = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitlDelta sc)
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = splitr sc
         }
splitAndCover SEmptyR (CExtendR c x) = 
  let sc = splitAndCover SEmptyR c in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = SExtendR (splitrDelta sc) x 
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = splitl sc
         ; splitr      = SExtendL (splitr sc) x
         }
splitAndCover SEmptyR (CExtendB c x) = 
  let sc = splitAndCover SEmptyR c in 
  record { splitlDelta = mapSig (first λ s → SExtendL s x) (splitlDelta sc)
         ; splitrDelta = SExtendL (splitrDelta sc) x
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig (first λ s → SExtendL s x) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = SExtendL (splitr sc) x
         }
splitAndCover (SExtendL s x) CEmptyL = 
  let sc = splitAndCover s CEmptyL in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = SExtendR (splitrDelta sc) x
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = splitl sc
         ; splitr      = SExtendL (splitr sc) x
         }
splitAndCover (SExtendL s x) CEmptyR = 
  let sc = splitAndCover s CEmptyR in 
  record { splitlDelta = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitlDelta sc)
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = splitr sc
         }
splitAndCover (SExtendL s x) (CExtendL c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitlDelta sc)
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = splitr sc
         }
splitAndCover (SExtendL s x) (CExtendR c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = SExtendR (splitrDelta sc) x
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = splitl sc
         ; splitr      = SExtendL (splitr sc) x
         }
splitAndCover (SExtendL s x) (CExtendB c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = mapSig (first λ s → SExtendL s x) (splitlDelta sc)
         ; splitrDelta = SExtendL (splitrDelta sc) x
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = mapSig (first λ s → SExtendL s x) (splitDelta sc)
         ; splitGamma  = splitGamma sc
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = SExtendL (splitr sc) x
         }
splitAndCover (SExtendR s x) CEmptyL = 
  let sc = splitAndCover s CEmptyL in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = SExtendR (splitrGamma sc) x 
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitGamma sc)
         ; splitl      = splitl sc
         ; splitr      = SExtendR (splitr sc) x
         }
splitAndCover (SExtendR s x) CEmptyR = 
  let sc = splitAndCover s CEmptyR in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitGamma sc)
         ; splitl      = SExtendR (splitl sc) x
         ; splitr      = splitr sc
         }
splitAndCover (SExtendR s x) (CExtendL c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = splitlDelta sc
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = splitrGamma sc
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendL s x)) (splitGamma sc)
         ; splitl      = SExtendR (splitl sc) x
         ; splitr      = splitr sc
         }
splitAndCover (SExtendR s x) (CExtendR c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = splitlDelta sc 
         ; splitrDelta = splitrDelta sc 
         ; splitrGamma = SExtendR (splitrGamma sc) x
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitGamma sc)
         ; splitl      = splitl sc
         ; splitr      = SExtendR (splitr sc) x
         }
splitAndCover (SExtendR s x) (CExtendB c x) = 
  let sc = splitAndCover s c in 
  record { splitlDelta = mapSig ((λ s → SExtendR s x) *** (λ s → SExtendR s x)) (splitlDelta sc)
         ; splitrDelta = splitrDelta sc
         ; splitrGamma = SExtendL (splitrGamma sc) x
         ; splitDelta  = splitDelta sc
         ; splitGamma  = mapSig (first λ s → SExtendL s x) (splitGamma sc)
         ; splitl      = SExtendL (splitl sc) x
         ; splitr      = SExtendR (splitr sc) x
         }

{-# COMPILE AGDA2HS splitAndCover #-}

------------------------------------------------------------------------
-- Syntax-directed rules

infix 3 _⊢ₛ_⇝ₙ_ _⊢ₛ_⇝ₙₛ_ _⊢ₛ_⇝ᵥ_ _⊢ₛ_⇝ₜ_ ⊢ₛ_⇝_

data _⊢ₛ_⇝ₙ_ : Context → Name x α → R.Name x α → Set where
  SNAME-DUP
    ----------------------------------
    : (∅ ▹ x) ¦ ∅ ⊢ₛ Only x ⇝ₙ R.Dup x
  SNAME
    -----------------------------------
    : ∅ ¦ (∅ ▹ x) ⊢ₛ Only x ⇝ₙ R.Only x

data _⊢ₛ_⇝ₙₛ_  {@0 α : Scope name} : Context → Names α → R.Names α → Set where
  SNIL 
    : {none : Atom α}
    ------------------------------------
    → Δ ¦ Γ ⊢ₛ NNil none ⇝ₙₛ R.NNil none

  SCONS
    : ∀ {n : Name x (∅ ▹ x)} {ns : Names β} {n′ ns′} {c : Cover (∅ ▹ x) β α} 
    → (s : Δ ⋈ Γ ≡ α) (let sc = splitAndCover s c)
    → Δₗᵣ,Δₗ,Γₗᵣ sc ¦ Γₗ sc     ⊢ₛ n  ⇝ₙ  n′ 
    → Δₗᵣ,Δᵣ sc     ¦ Γₗᵣ,Γᵣ sc ⊢ₛ ns ⇝ₙₛ ns′
    --------------------------------------------------------------
    → Δ ¦ Γ ⊢ₛ NCons (MkPair c n ns) ⇝ₙₛ R.NCons (MkPair c n′ ns′) 

data _⊢ₛ_⇝ᵥ_ {@0 α : Scope name} : Context → Val α → R.Val α → Set where
  SVAR 
    : {n : Name x α} {n′ : R.Name x α}
    → Δ ¦ Γ  ⊢ₛ n ⇝ₙ n′
    ---------------------------------
    → Δ ¦ Γ  ⊢ₛ Var n ⇝ᵥ R.Var n′

data _⊢ₛ_⇝ₜ_ {@0 α : Scope name} : Context → Term α → R.Term α → Set where
  SRETURN
    : ∀ {v v′} 
    → Δ ¦ Γ ⊢ₛ v ⇝ᵥ v′
    ----------------------------------
    → Δ ¦ Γ ⊢ₛ Return v ⇝ₜ R.Return v′

  SAPPDEF 
    : ∀ {f p ns ns′} 
    → Δ ¦ Γ ⊢ₛ ns ⇝ₙₛ ns′
    --------------------------------------------
    → Δ ¦ Γ ⊢ₛ AppDef f p ns ⇝ₜ R.AppDef f p ns′

  SBIND 
    : ∀ {fvₗ fvᵣ xs r tₗ tₗ′ tᵣ tᵣ′} 
      {c : Cover fvₗ fvᵣ α} {p : xs ⊆ β} 
    → (s : Δ ⋈ Γ ≡ α) (let sc = splitAndCover s c) 
    → Δₗᵣ,Δₗ,Γₗᵣ sc ¦ Γₗ           sc ⊢ₛ tₗ ⇝ₜ tₗ′
    → Δₗᵣ,Δᵣ     sc ¦ Γₗᵣ,Γᵣ sc <> xs ⊢ₛ tᵣ ⇝ₜ tᵣ′
    ---------------------------------------------------------------------------
    → Δ ¦ Γ ⊢ₛ Bind r (MkPair c tₗ (MkBinder p tᵣ)) ⇝ₜ 
               R.Bind r (MkPair c tₗ′ (MkBinder < SEmptyR > (R.drops r p tᵣ′)))

data ⊢ₛ_⇝_ : Definition → R.Definition → Set where
  SDEF 
    : ∀ {varsScope freeScope vars t t′} {varsUsage : freeScope ⊆ varsScope}
    → Δ ¦ Γ ⊢ₛ t ⇝ₜ t′ 
    -----------------------------------------------------------------------
    → ⊢ₛ MkDef vars varsUsage t ⇝ R.MkDef vars (R.drops' vars varsUsage t′)

------------------------------------------------------------------------
-- Implementation of the rules 
--
-- Produces a value proof pair.

perceusName 
  : ∀ {@0 Δ Γ} 
  → Δ ⋈ Γ ≡ α
  → (n : Name x α)
  → ∃[ n′ ∈ R.Name x α ] Δ ¦ Γ ⊢ₛ n ⇝ₙ n′
perceusName SEmptyL              (Only x) = _ ⟨ SNAME ⟩
perceusName SEmptyR              (Only x) = _ ⟨ SNAME-DUP ⟩
perceusName (SExtendL SEmptyL x) (Only x) = _ ⟨ SNAME-DUP ⟩
perceusName (SExtendL SEmptyR x) (Only x) = _ ⟨ SNAME-DUP ⟩
perceusName (SExtendR SEmptyL x) (Only x) = _ ⟨ SNAME ⟩
perceusName (SExtendR SEmptyR x) (Only x) = _ ⟨ SNAME ⟩ 

{-# COMPILE AGDA2HS perceusName  #-}

perceusNames
  : Δ ⋈ Γ ≡ α
  → (ns : Names α)
  → ∃[ ns′ ∈ R.Names α ] Δ ¦ Γ ⊢ₛ ns ⇝ₙₛ ns′
perceusNames SEmptyL (NNil None) = _ ⟨ SNIL ⟩
perceusNames SEmptyR (NNil None) = _ ⟨ SNIL ⟩
perceusNames s (NCons (MkPair c (Only x) ns)) = 
  let 
    sc = splitAndCover s c
    _ ⟨ proofₗ ⟩ = perceusName (splitl sc) (Only x) 
    _ ⟨ proofᵣ ⟩ = perceusNames (splitr sc) ns 
  in _ ⟨ SCONS s proofₗ proofᵣ ⟩

{-# COMPILE AGDA2HS perceusNames  #-}

perceusVal 
  : Δ ⋈ Γ ≡ α
  → (v : Val α)
  → ∃[ v′ ∈ R.Val α ] Δ ¦ Γ ⊢ₛ v ⇝ᵥ v′
perceusVal s (Var (Only x)) =
    let _ ⟨ proof ⟩ = perceusName s (Only x) in 
    _ ⟨ SVAR proof ⟩ 

{-# COMPILE AGDA2HS perceusVal  #-}

perceusTerm 
  : Δ ⋈ Γ ≡ α
  → (t : Term α) 
  → ∃[ t′ ∈ R.Term α ] Δ ¦ Γ ⊢ₛ t ⇝ₜ t′
perceusTerm s (Return v) = 
  let _ ⟨ proof ⟩ = perceusVal s v
  in  _ ⟨ SRETURN proof ⟩
perceusTerm s (AppDef f p xs) = 
  let _ ⟨ proof ⟩ = perceusNames s xs in
  _ ⟨ SAPPDEF proof ⟩
perceusTerm s (Bind {β = β} r (MkPair c tl b)) = 
  let 
    sc = splitAndCover s c
    _ ⟨ proofₗ ⟩ = perceusTerm (splitl sc) tl
    _ ⟨ proofᵣ ⟩ = perceusTerm (splitJoinRight (rezzSplitLeft (proj₂ (usage b)) r) (splitr sc)) (body b)
  in _ ⟨ SBIND s proofₗ proofᵣ ⟩

{-# COMPILE AGDA2HS perceusTerm  #-}

perceus : (f : Definition) → ∃[ f′ ∈ R.Definition ] ⊢ₛ f ⇝ f′
perceus f = 
  let _ ⟨ proof ⟩ = perceusTerm SEmptyL (term f)
  in  _ ⟨ SDEF proof ⟩

{-# COMPILE AGDA2HS perceus #-}

