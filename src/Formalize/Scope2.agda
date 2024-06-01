
module Formalize.Scope2 where

open import Haskell.Prelude using
  ( Semigroup; _<>_;
    Monoid;  mempty; super; mappend; mconcat;
    _∷_; [];
    _≡_; refl;
    _×_; _,_; fst; snd; 
    ⊥
  )
open import Haskell.Extra.Erase
open import Haskell.Extra.Sigma
open import Formalize.Utils.Erase
open import Haskell.Law.Equality
open import Haskell.Law.Semigroup.Def
open import Haskell.Law.Monoid.Def



open import Formalize.Scope using 
  ( Scope; ∅; _▹_; _◃_
  ; <>-▹-assoc; ▹-<>≡<>-◃
  ; rezzAppendLeft
  ; iSemigroupScope; iLawfulSemigroupScope
  ; iMonoidScope; iLawfulMonoidScope
  ) public

private variable
  @0 name : Set
  @0 x y  : name
  @0 α β γ δ ε ζ : Scope name
  @0 α′ β′ γ′ δ′ ε′ ζ′ : Scope name

data Sub {@0 name : Set} : @0 Scope name → @0 Scope name → Set where
  Empty   : Sub ∅ ∅
  Keep    : Sub α β → (@0 x : name) → Sub (α ▹ x) (β ▹ x)
  Discard : Sub α β → (@0 x : name) → Sub α       (β ▹ x)
  
{-# COMPILE AGDA2HS Sub #-}

infixl 20 _keep_ _discard_

pattern empty = Empty
pattern _keep_ Φ x = Keep Φ x 
pattern _discard_ Φ x = Discard Φ x 

infix 5 _⊆_

_⊆_ = Sub

{-# COMPILE AGDA2HS _⊆_ inline #-}

subKeeps : α ⊆ β → Rezz (Scope name) γ → (α <> γ) ⊆ (β <> γ)
subKeeps p (rezz ∅) = p
subKeeps p (rezz (xs ▹ x)) = subKeeps p (rezz xs) keep x

{-# COMPILE AGDA2HS subKeeps #-}

subDiscards : α ⊆ β → Rezz (Scope name) γ → α ⊆ (β <> γ)
subDiscards p (rezz ∅) = p
subDiscards p (rezz (xs ▹ x)) = subDiscards p (rezz xs) discard x

{-# COMPILE AGDA2HS subDiscards #-}

infixl 20 _keeps_ _discards_

_keeps_ = subKeeps

{-# COMPILE AGDA2HS _keeps_ inline #-}

_discards_ = subDiscards

{-# COMPILE AGDA2HS _discards_ inline #-}

-- @0 subKeeps : α ⊆ β → (γ : Scope name) → (α <> α′) ⊆ (β <> β′)

subAppend : α ⊆ β → α′ ⊆ β′ → (α <> α′) ⊆ (β <> β′)
subAppend p empty = p
subAppend p (q keep x) = subAppend p q keep x
subAppend p (q discard x) = subAppend p q discard x

subRefl : Rezz (Scope name) α → α ⊆ α
subRefl (rezz ∅) = empty
subRefl (rezz (xs ▹ x)) = subRefl (rezz xs) keep x

@0 ⊆-refl : {α : Scope name} → α ⊆ α 
⊆-refl {α = ∅} = empty
⊆-refl {α = α ▹ x} = ⊆-refl keep x

subEmpty : Rezz (Scope name) α → ∅ ⊆ α
subEmpty (rezz ∅) = empty 
subEmpty (rezz (xs ▹ x)) = subEmpty (rezz xs) discard x 

@0 ⊆-empty : {α : Scope name} → ∅ ⊆ α 
⊆-empty {α = ∅} = empty
⊆-empty {α = α ▹ x} = ⊆-empty discard x

@0 law-⊆-empty : (Φ : ∅ ⊆ α) → Φ ≡ ⊆-empty
law-⊆-empty empty = refl
law-⊆-empty (Φ discard x) = cong (_discard x) (law-⊆-empty Φ) 

subTrans : α ⊆ β → β ⊆ γ → α ⊆ γ
subTrans empty empty = empty
subTrans p (q discard x) = subTrans p q discard x
subTrans (p keep x) (q keep x) = subTrans p q keep x
subTrans (p discard x) (q keep x) = subTrans p q discard x

rezzSubLeft : α ⊆ β → Rezz (Scope name) α
rezzSubLeft empty = rezz ∅
rezzSubLeft (p keep x) = rezzCong (_▹ x) (rezzSubLeft p)
rezzSubLeft (p discard x) = rezzSubLeft p

{-# COMPILE AGDA2HS rezzSubLeft #-}

rezzSubRight : α ⊆ β → Rezz (Scope name) β 
rezzSubRight empty = rezz ∅
rezzSubRight (p keep x) = rezzCong (_▹ x) (rezzSubRight p)
rezzSubRight (p discard x) = rezzCong (_▹ x) (rezzSubRight p)

{-# COMPILE AGDA2HS rezzSubRight #-}


In : @0 name → @0 Scope name → Set
In x α = (∅ ▹ x) ⊆ α

{-# COMPILE AGDA2HS In #-}

infix 5 _∈_

_∈_ = In   

{-# COMPILE AGDA2HS _∈_ inline #-}

data Cover {@0 name : Set} : {@0 α β γ : Scope name} → @0 α ⊆ γ → @0 β ⊆ γ → Set where
  CEmpty  : Cover empty empty
  CLeft   : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ keep x)    (Θ discard x) 
  CRight  : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ discard x) (Θ keep x) 
  CBoth   : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (@0 x : name) → Cover (Φ keep x)    (Θ keep x) 

{-# COMPILE AGDA2HS Cover #-}

infixl 20 _left_ _right_ _both_

pattern empty = CEmpty
pattern _left_ c x = CLeft c x 
pattern _right_ c x = CRight c x 
pattern _both_ c x = CBoth c x 

coverLefts : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (r : Rezz (Scope name) δ) → Cover (Φ keeps r) (Θ discards r)
coverLefts c (rezz ∅) = c
coverLefts c (rezz (xs ▹ x)) = coverLefts c (rezz xs) left x

coverRights : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (r : Rezz (Scope name) δ) → Cover (Φ discards r) (Θ keeps r)
coverRights c (rezz ∅) = c
coverRights c (rezz (xs ▹ x)) = coverRights c (rezz xs) right x

coverBoths : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → (r : Rezz (Scope name) δ) → Cover (Φ keeps r) (Θ keeps r)
coverBoths c (rezz ∅) = c
coverBoths c (rezz (xs ▹ x)) = coverBoths c (rezz xs) both x

infixl 20 _lefts_ _rights_ _boths_

_lefts_ = coverLefts

{-# COMPILE AGDA2HS _lefts_ inline #-}

_rights_ = coverRights

{-# COMPILE AGDA2HS _rights_ inline #-}

_boths_ = coverBoths

{-# COMPILE AGDA2HS _boths_ inline #-}


coverAppend : 
  {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} {@0 Φ′ : α′ ⊆ γ′} {@0 Θ′ : β′ ⊆ γ′} → 
  Cover Φ Θ → Cover Φ′ Θ′ → Cover (subAppend Φ Φ′) (subAppend Θ Θ′ )
coverAppend c1 empty = c1
coverAppend c1 (c2 left x) = coverAppend c1 c2 left x
coverAppend c1 (c2 right x) = coverAppend c1 c2 right x
coverAppend c1 (c2 both x) = coverAppend c1 c2 both x

rezzCover : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → Rezz (Scope name) γ
rezzCover empty = rezz ∅
rezzCover (cover left x) = rezzCong (_▹ x) (rezzCover cover)
rezzCover (cover right x) = rezzCong (_▹ x) (rezzCover cover)
rezzCover (cover both x) = rezzCong (_▹ x) (rezzCover cover)

coverEmptyLeft : Rezz (Scope name) γ → Cover {γ = γ} ⊆-empty ⊆-refl
coverEmptyLeft (rezz ∅) = empty
coverEmptyLeft (rezz (xs ▹ x)) = coverEmptyLeft (rezz xs) right x

coverEmptyRight : Rezz (Scope name) γ → Cover {γ = γ} ⊆-refl ⊆-empty
coverEmptyRight (rezz ∅) = empty
coverEmptyRight (rezz (xs ▹ x)) = coverEmptyRight (rezz xs) left x

data Split {@0 name : Set} : {@0 α β γ : Scope name} → @0 α ⊆ γ → @0 β ⊆ γ → Set where
  SEmpty  : Split Empty Empty
  SLeft   : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → (@0 x : name) → Split (Φ keep x)    (Θ discard x) 
  SRight  : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → (@0 x : name) → Split (Φ discard x) (Θ keep x) 

pattern empty = SEmpty
pattern _left_ c x = SLeft c x 
pattern _right_ c x = SRight c x 

splitLefts : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → (r : Rezz (Scope name) δ) → Split (Φ keeps r) (Θ discards r)
splitLefts c (rezz ∅) = c
splitLefts c (rezz (xs ▹ x)) = splitLefts c (rezz xs) left x

splitRights : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → (r : Rezz (Scope name) δ) → Split (Φ discards r) (Θ keeps r)
splitRights c (rezz ∅) = c
splitRights c (rezz (xs ▹ x)) = splitRights c (rezz xs) right x

_leftsₛ_ = splitLefts

{-# COMPILE AGDA2HS _leftsₛ_ inline #-}

_rightsₛ_ = splitRights

{-# COMPILE AGDA2HS _rightsₛ_ inline #-}

splitAppend : 
  {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} {@0 Φ′ : α′ ⊆ γ′} {@0 Θ′ : β′ ⊆ γ′} → 
  Split Φ Θ → Split Φ′ Θ′ → Split (subAppend Φ Φ′) (subAppend Θ Θ′ )
splitAppend c1 empty = c1
splitAppend c1 (c2 left x) = splitAppend c1 c2 left x
splitAppend c1 (c2 right x) = splitAppend c1 c2 right x

rezzSplit : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → Rezz (Scope name) γ
rezzSplit empty = rezz ∅
rezzSplit (s left x) = rezzCong (_▹ x) (rezzSplit s)
rezzSplit (s right x) = rezzCong (_▹ x) (rezzSplit s)

rezzSplitRight : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → Rezz (Scope name) β
rezzSplitRight empty = rezz ∅
rezzSplitRight (s left x) = rezzSplitRight s
rezzSplitRight (s right x) = rezzCong (_▹ x) (rezzSplitRight s)

rezzSplitLeft : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → Rezz (Scope name) α
rezzSplitLeft empty = rezz ∅
rezzSplitLeft (s left x) = rezzCong (_▹ x) (rezzSplitLeft s)
rezzSplitLeft (s right x) = (rezzSplitLeft s)

splitEmptyLeft : Rezz (Scope name) γ → Split {γ = γ} ⊆-empty ⊆-refl
splitEmptyLeft (rezz ∅) = empty
splitEmptyLeft (rezz (xs ▹ x)) = splitEmptyLeft (rezz xs) right x

splitEmptyRight : Rezz (Scope name) γ → Split {γ = γ} ⊆-refl ⊆-empty
splitEmptyRight (rezz ∅) = empty
splitEmptyRight (rezz (xs ▹ x)) = splitEmptyRight (rezz xs) left x

splitComm : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Split Φ Θ → Split Θ Φ 
splitComm empty = empty
splitComm (p left x) = splitComm p right x
splitComm (p right x) = splitComm p left x

subToSplit : (Φ : α ⊆ γ) → Σ0[ β ∈ Scope name ] Σ[ Θ ∈ β ⊆ γ ] Split Φ Θ
subToSplit empty = < empty , empty >
subToSplit (p keep x) = 
  let < p , s > = subToSplit p in 
  < p discard x , s left x >
subToSplit (p discard x) = 
  let < p ,  s > = subToSplit p in 
  < p keep x , s right x >

∅-Split-injective : {Φ : α ⊆ β} → Split ⊆-empty Φ → α ≡ β
∅-Split-injective empty = refl
∅-Split-injective (p right x) rewrite ∅-Split-injective p = refl

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
