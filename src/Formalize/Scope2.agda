
module Formalize.Scope2 where

open import Haskell.Prelude using
  ( Semigroup; _<>_;
    Monoid;  mempty; super; mappend; mconcat;
    _∷_; [];
    _≡_; refl;
    _×_; _,_; fst; snd
  )
open import Haskell.Extra.Erase
open import Formalize.Utils.Erase
open import Haskell.Law.Equality
open import Haskell.Law.Semigroup.Def
open import Haskell.Law.Monoid.Def


open import Formalize.Scope using 
  ( Scope; ∅; _▹_ 
  ; iSemigroupScope; iLawfulSemigroupScope
  ; iMonoidScope; iLawfulMonoidScope
  ) public

private variable
  @0 name : Set
  @0 x y  : name
  @0 α β γ δ ε ζ : Scope name


data Thinning {@0 name : Set} : @0 Scope name → @0 Scope name → Set where
  Empty   : Thinning ∅ ∅
  Keep    : Thinning α β → (@0 x : name) → Thinning (α ▹ x) (β ▹ x)
  Discard : Thinning α β → (@0 x : name) → Thinning α       (β ▹ x)
  
{-# COMPILE AGDA2HS Thinning #-}

infixl 20 _keep_ _discard_

pattern empty = Empty
pattern _keep_ Φ x = Keep Φ x 
pattern _discard_ Φ x = Discard Φ x 

infix 5 _⊆_

_⊆_ = Thinning

{-# COMPILE AGDA2HS _⊆_ inline #-}

In : @0 name → @0 Scope name → Set
In x α = (∅ ▹ x) ⊆ α

{-# COMPILE AGDA2HS In #-}

infix 5 _∈_

_∈_ = In   

{-# COMPILE AGDA2HS _∈_ inline #-}

data Cover {@0 name : Set} : {@0 α β γ : Scope name} → @0 α ⊆ γ → @0 β ⊆ γ → Set where
  CEmpty  : Cover Empty Empty
  CLeft   : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ keep x)    (Φ discard x) 
  CRight  : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ discard x) (Φ keep x) 
  CBoth   : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ keep x)    (Φ keep x) 

{-# COMPILE AGDA2HS Cover #-}

infixl 20 _left_ _right_ _both_

pattern empty = CEmpty
pattern _left_ c x = CLeft c x 
pattern _right_ c x = CRight c x 
pattern _both_ c x = CBoth c x 

rezzCover : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → Rezz (Scope name) γ
rezzCover empty = rezz ∅
rezzCover (CLeft cover x) = rezzCong (_▹ x) (rezzCover cover)
rezzCover (CRight cover x) = rezzCong (_▹ x) (rezzCover cover)
rezzCover (CBoth cover x) = rezzCong (_▹ x) (rezzCover cover)

-- of the variables.
record Binder (@0 β : Scope name) (f : @0 Scope name → Set) (@0 α : Scope name) : Set where
  constructor  MkBinder
  field
    @0 {usedScope} : Scope name
    -- TODO: maybe add a tagInfo field.
    -- tagInfo : All (λ _ → TagInfo) β
    usage          : usedScope ⊆ β
    body           : f (α <> usedScope)

{-# COMPILE AGDA2HS Binder deriving Show #-}

open Binder public

-- Relevant pair. Combine two constructs (f and g). A cover tells
-- which variables goes where.
record Pair (f g : @0 Scope name → Set) (@0 α : Scope name) : Set where
  constructor MkPair
  field
    @0 {fvₗ} : Scope name
    @0 {fvᵣ} : Scope name
    @0 {thₗ} : fvₗ ⊆ α
    @0 {thᵣ} : fvᵣ ⊆ α
    cover    : Cover thₗ thᵣ
    outl     : f fvₗ
    outr     : g fvᵣ

{-# COMPILE AGDA2HS Pair deriving Show #-}

open Pair public

data Atom {@0 name : Set} : @0 Scope name → Set where
  None : Atom ∅

{-# COMPILE AGDA2HS Atom deriving Show #-}
