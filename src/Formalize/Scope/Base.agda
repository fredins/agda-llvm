
module Formalize.Scope.Base where

open import Haskell.Prelude using (_×_; _,_; _∘_; _≡_; refl) renaming (mempty to ∅) 
open import Haskell.Extra.Erase 
open import Haskell.Law

open import Scope 
open import Formalize.Utils.Erase

private variable
  a            : Set
  @0 name      : Set
  @0 α β γ δ   : Scope name
  @0 x         : name

  -- @0 name : Set
  -- @0 α β  : Scope name
  -- @0 x    : name


--------------------------------------------------------------------------------
-- * Cover.

-- From "Everybody’s Got To Be Somewhere" by Connor McBride, 2018. Also, see 
-- Jesper Cockx's blogpost for a short introduction: 
-- https://jesper.sikanda.be/posts/1001-syntax-representations.html.
data Cover {@0 name : Set} : (@0 α β γ : Scope name) → Set where
  CDone  : Cover ∅ ∅ ∅
  CLeft  : Cover α β γ → Cover (x ◃ α) β       (x ◃ γ)
  CRight : Cover α β γ → Cover α       (x ◃ β) (x ◃ γ)
  CBoth  : Cover α β γ → Cover (x ◃ α) (x ◃ β) (x ◃ γ)


{-# COMPILE AGDA2HS Cover deriving Show #-}

done  : Cover {name = name} ∅ ∅ ∅
left  : Cover α β γ → Cover (x ◃ α) β       (x ◃ γ)
right : Cover α β γ → Cover α       (x ◃ β) (x ◃ γ)
both  : Cover α β γ → Cover (x ◃ α) (x ◃ β) (x ◃ γ)

done  = CDone

{-# COMPILE AGDA2HS done deriving Show #-}

left  = CLeft

{-# COMPILE AGDA2HS left deriving Show #-}

right = CRight

{-# COMPILE AGDA2HS right deriving Show #-}

both  = CBoth

{-# COMPILE AGDA2HS both deriving Show #-}

opaque 
  unfolding Scope
  @0 ∅-Cover-injective : Cover ∅ β γ → β ≡ γ
  ∅-Cover-injective CDone      = refl
  ∅-Cover-injective (CRight c) = cong (bind _) (∅-Cover-injective c)

--------------------------------------------------------------------------------
-- * Three split.

opaque 

  Split3 : {@0 name : Set} → Scope name → Scope name → Scope name → Scope name → Set
  Split3 {name} α β γ δ = Σ0[ tmp ∈ Scope name ]  (α ⋈ tmp ≡ δ) × (β ⋈ γ ≡ tmp)

  syntax Split3 α β γ δ = α ⋈ β ⋈ γ ≡ δ

  split3EmptyLefts : ∅ ⋈ ∅ ⋈ α ≡ α
  split3EmptyLefts = < splitEmptyLeft , splitEmptyLeft >

  split3EmptyRights : α ⋈ ∅ ⋈ ∅  ≡ α
  split3EmptyRights = < splitEmptyRight , splitEmptyLeft  >

  split3Left : α ⋈ β ⋈ γ ≡ δ → Σ0[ ε ∈ Scope name ] α ⋈ ε ≡ δ
  split3Left < splitL , splitR > = < splitL >

  split3Right : α ⋈ β ⋈ γ ≡ δ → Σ0[ ε ∈ Scope name ] β ⋈ γ ≡ ε 
  split3Right < splitL , splitR > = < splitR >

  split3Parts : α ⋈ β ⋈ γ ≡ δ → Σ0[ ε ∈ Scope name ] (α ⋈ ε ≡ δ) × (β ⋈ γ ≡ ε)
  split3Parts split = split

  split3Comm : α ⋈ β ⋈ γ ≡ δ → α ⋈ γ ⋈ β ≡ δ 
  split3Comm < splitL , splitR > = < splitL , splitComm splitR > 

  split3Assoc : α ⋈ β ⋈ γ ≡ δ → β ⋈ γ ⋈ α ≡ δ
  split3Assoc < splitL , splitR > = splitAssoc splitR (splitComm splitL)


opaque 
  unfolding Split3 Split Sub

  sub3Left : α ⋈ β ⋈ γ ≡ δ → α ⊆ δ
  sub3Left < splitL , splitR > = < splitL > 

  sub3Middle : α ⋈ β ⋈ γ ≡ δ → β ⊆ δ
  sub3Middle < splitL , splitR >  = subTrans (subLeft splitR) (subRight splitL) 

  sub3Right : α ⋈ β ⋈ γ ≡ δ → γ ⊆ δ
  sub3Right < splitL , splitR >  = subTrans (subRight splitR) (subRight splitL) 

  inSplit3Case : α ⋈ β ⋈ γ ≡ δ → x ∈ δ → (x ∈ α → a) → (x ∈ β → a) → (x ∈ γ → a) → a
  inSplit3Case < p₁         , p₂         > < EmptyR    > f g h = inSplitCase p₁ subRefl f λ q → inSplitCase p₂ q g h
  inSplit3Case < p₁         , p₂         > < ConsL _ q > f g h = inSplitCase p₁ inHere f λ q → inSplitCase p₂ q g h
  inSplit3Case < EmptyL     , p₂         > < ConsR _ q > f g h = inSplitCase p₂ (inThere < q >) g h
  inSplit3Case < EmptyR     , EmptyL     > < ConsR _ q > f g h = f (inThere < q >)
  inSplit3Case < EmptyR     , EmptyR     > < ConsR _ q > f g h = f (inThere < q >)
  inSplit3Case < ConsL _ p₁ , p₂         > < ConsR _ q > f g h = inSplit3Case < p₁ , p₂ > < q > (f ∘ inThere) g h
  inSplit3Case < ConsR _ p₁ , EmptyL     > < ConsR _ q > f g h = inSplitCase p₁ < q > f (h ∘ inThere)
  inSplit3Case < ConsR _ p₁ , EmptyR     > < ConsR _ q > f g h = inSplitCase p₁ < q > f (g ∘ inThere)
  inSplit3Case < ConsR _ p₁ , ConsL _ p₂ > < ConsR _ q > f g h = inSplit3Case < p₁ , p₂ > < q > f (g ∘ inThere) h
  inSplit3Case < ConsR _ p₁ , ConsR _ p₂ > < ConsR _ q > f g h = inSplit3Case < p₁ , p₂ > < q > f g (h ∘ inThere) 
