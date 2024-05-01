
--------------------------------------------------------------------------
-- Scoping. Heavily inspired by github.com/jespercockx/scope, but it uses
-- co-de-Bruijn indices (McBride 2018), backwards lists, and it doesn't use 
-- opaque definitions.

module Formalize.Scope where

open import Haskell.Prelude using (Semigroup; _<>_; Monoid;  mempty; super; mappend; mconcat; _∷_; []; _≡_; refl; _×_; _,_; fst; snd)
open import Haskell.Extra.Erase
open import Formalize.Utils.Erase 
open import Haskell.Law.Equality 
open import Haskell.Law.Semigroup.Def
open import Haskell.Law.Monoid.Def

private variable
  @0 name : Set
  @0 x y  : name

data Scope (@0 name : Set) : Set where
  SEmpty  : Scope name
  SExtend : Scope name → Erase name → Scope name

{-# COMPILE AGDA2HS Scope deriving Show #-}

pattern ∅ = SEmpty

pattern _▹_ α x = SExtend α (Erased x)

infixl 5 _▹_

instance 
  iSemigroupScope : Semigroup (Scope name)
  iSemigroupScope ._<>_ xs ∅        = xs
  iSemigroupScope ._<>_ xs (ys ▹ y) = (xs <> ys) ▹ y

  {-# COMPILE AGDA2HS iSemigroupScope #-}
  
  iLawfulSemigroupScope : IsLawfulSemigroup (Scope name)
  iLawfulSemigroupScope .associativity xs ys ∅        = refl
  iLawfulSemigroupScope .associativity xs ys (zs ▹ x) 
    rewrite sym (associativity xs ys zs) 
    = refl 

  iMonoidScope : Monoid (Scope name)
  iMonoidScope .mempty  = ∅
  iMonoidScope .super   = iSemigroupScope
  iMonoidScope .mappend = _<>_
  iMonoidScope .mconcat []       = mempty
  iMonoidScope .mconcat (x ∷ xs) = x <> mconcat xs 

  {-# COMPILE AGDA2HS iMonoidScope #-}

  iLawfulMonoidScope : IsLawfulMonoid (Scope name)
  iLawfulMonoidScope .rightIdentity ∅ = refl
  iLawfulMonoidScope .rightIdentity (xs ▹ x) rewrite rightIdentity xs = refl
  iLawfulMonoidScope .leftIdentity ∅ = refl
  iLawfulMonoidScope .leftIdentity (xs ▹ x) rewrite leftIdentity xs = refl
  iLawfulMonoidScope .concatenation [] = refl
  iLawfulMonoidScope .concatenation (x ∷ xs) rewrite concatenation xs = refl

cons : @0 name → Scope name → Scope name
cons x α = (∅ ▹ x) <> α

{-# COMPILE AGDA2HS cons #-}

infixr 5 cons

syntax cons x α = x ◃ α

private variable 
  @0 α β γ δ ε ζ : Scope name 

rezzExtend : Rezz (Scope name) α → Rezz (Scope name) (α ▹ x)
rezzExtend = rezzCong (_▹ _)

{-# COMPILE AGDA2HS rezzExtend #-}

data NonEmpty {name : Set} : @0 Scope name → Set where
  instance itsNonEmpty : NonEmpty (α ▹ x)

scopeInit : (α : Scope name) → @0 ⦃ NonEmpty α ⦄ → Scope name
scopeInit (SExtend xs x) = xs

{-# COMPILE AGDA2HS scopeInit #-}

rezzInit : Rezz (Scope name) (α ▹ x) → Rezz (Scope name) α
rezzInit {α = α} (Rezzed xs p) = 
  Rezzed (scopeInit xs) 
    (subst (λ xs → ⦃ @0 _ : NonEmpty xs ⦄ → scopeInit xs ≡ α) (sym p) refl)
  where instance 
  @0 _ : NonEmpty xs
  _ = subst0 NonEmpty (sym p) itsNonEmpty

{-# COMPILE AGDA2HS rezzInit #-}

<>-▹-assoc : (α β : Scope name) (x : name) → α <> (β ▹ x) ≡ (α <> β) ▹ x 
<>-▹-assoc α ∅ x = refl
<>-▹-assoc α (β ▹ y) x rewrite <>-▹-assoc α β x = refl

▹-<>≡<>-◃ : (α : Scope name) (x : name) (β : Scope name) → (α ▹ x) <> β ≡ α <> (x ◃ β) 
▹-<>≡<>-◃ α x β = begin 
  (α ▹ x) <> β 
  ≡⟨ sym (associativity α (∅ ▹ x) β) ⟩ 
  α <> (x ◃ β)
  ∎ 

-- Cover is a disjoint union.
data Cover {@0 name : Set} : (@0 α β γ : Scope name) → Set where
  CEmptyL  : Cover ∅ β β
  CEmptyR  : Cover α ∅ α
  CExtendL : Cover α β γ → (@0 x : name) → Cover (α ▹ x) β (γ ▹ x)
  CExtendR : Cover α β γ → (@0 x : name) → Cover α (β ▹ x) (γ ▹ x)
  CExtendB : Cover α β γ → (@0 x : name) → Cover (α ▹ x) (β ▹ x) (γ ▹ x)

{-# COMPILE AGDA2HS Cover deriving Show #-}

coverJoinRight : Rezz _ δ → Cover α β γ → Cover α (β <> δ) (γ <> δ)
coverJoinRight (rezz ∅)       s = s
coverJoinRight (rezz (xs ▹ x)) s = CExtendR (coverJoinRight (rezz xs) s) x

{-# COMPILE AGDA2HS coverJoinRight #-}

-- Split is a partition so overlap is disallowed.
data Split {@0 name : Set} : (@0 α β γ : Scope name) → Set where
  SEmptyL  : Split ∅ β β
  SEmptyR  : Split α ∅ α
  SExtendL : Split α β γ → (@0 x : name) → Split (α ▹ x) β (γ ▹ x)
  SExtendR : Split α β γ → (@0 x : name) → Split α (β ▹ x) (γ ▹ x)

{-# COMPILE AGDA2HS Split deriving Show #-}

syntax Split α β γ = α ⋈ β ≡ γ

splitComm : α ⋈ β ≡ γ → β ⋈ α ≡ γ
splitComm SEmptyL = SEmptyR
splitComm SEmptyR = SEmptyL
splitComm (SExtendL s x) = SExtendR (splitComm s) x
splitComm (SExtendR s x) = SExtendL (splitComm s) x

{-# COMPILE AGDA2HS splitComm #-}

splitAssoc
  : α ⋈ β ≡ γ
  → γ ⋈ δ ≡ ε
  → Σ0 _ λ ζ → (α ⋈ ζ ≡ ε) × (β ⋈ δ ≡ ζ)
splitAssoc SEmptyL q = < SEmptyL , q >
splitAssoc SEmptyR q = < q , SEmptyL >
splitAssoc p SEmptyR = < p , SEmptyR >
splitAssoc (SExtendL p x) (SExtendL q x) = 
  let < r , s > = splitAssoc p q
  in  < SExtendL r x , s >
splitAssoc (SExtendR p x) (SExtendL q x) = 
  let < r , s > = splitAssoc p q
  in  < SExtendR r x , SExtendL s x >
splitAssoc p (SExtendR q x) =
  let < r , s > = splitAssoc p q
  in  < SExtendR r x , SExtendR s x >

{-# COMPILE AGDA2HS splitAssoc #-}

splitJoinRight : Rezz _ δ → α ⋈ β ≡ γ → α ⋈ (β <> δ) ≡ (γ <> δ)
splitJoinRight (rezz ∅)       s = s
splitJoinRight (rezz (xs ▹ x)) s = SExtendR (splitJoinRight (rezz xs) s) x

{-# COMPILE AGDA2HS splitJoinRight #-}

splitJoinLeft : Rezz _ δ → α ⋈ β ≡ γ → (α <> δ) ⋈ β ≡ (γ <> δ)
splitJoinLeft (rezz ∅)       s = s
splitJoinLeft (rezz (xs ▹ x)) s = SExtendL (splitJoinLeft (rezz xs) s) x

{-# COMPILE AGDA2HS splitJoinLeft #-}

rezzSplit : α ⋈ β ≡ γ → Rezz _ γ → Rezz _ α × Rezz _ β
rezzSplit SEmptyL r = rezz ∅ , r
rezzSplit SEmptyR r = r , rezz ∅
rezzSplit (SExtendL s x) r = 
  let r1 , r2 = rezzSplit s (rezzInit r) in  
  rezzExtend r1 , r2
rezzSplit (SExtendR s x) r = 
  let r1 , r2 = rezzSplit s (rezzInit r) in  
  r1 , rezzExtend r2

{-# COMPILE AGDA2HS rezzSplit #-}

rezzSplitLeft : α ⋈ β ≡ γ → Rezz _ γ → Rezz _ α 
rezzSplitLeft s r = fst (rezzSplit s r) 

{-# COMPILE AGDA2HS rezzSplitLeft #-}

rezzSplitRight : α ⋈ β ≡ γ → Rezz _ γ → Rezz _ β
rezzSplitRight s r = snd (rezzSplit s r) 

{-# COMPILE AGDA2HS rezzSplitRight #-}

@0 ∅-⋈-injective : ∅ ⋈ α ≡ β → α ≡ β
∅-⋈-injective SEmptyL = refl
∅-⋈-injective SEmptyR = refl
∅-⋈-injective (SExtendR s x) rewrite ∅-⋈-injective s = refl

-- A three split.
Split3 : {@0 name : Set} (@0 α β γ δ : Scope name) → Set
Split3 {name} α β γ δ = Σ0[ ζ ∈ Scope name ] (α ⋈ ζ ≡ δ) × (β ⋈ γ ≡ ζ)

{-# COMPILE AGDA2HS Split3 #-}

syntax Split3 α β γ δ = α ⋈ β ⋈ γ ≡ δ

Sub : {@0 name : Set} (@0 α β : Scope name) → Set
Sub α β = Σ0[ δ ∈ Scope _ ] α ⋈ δ ≡ β

{-# COMPILE AGDA2HS Sub #-}

syntax Sub α β = α ⊆ β

In : @0 name → @0 Scope name → Set
In x α = (∅ ▹ x) ⊆ α

{-# COMPILE AGDA2HS In #-}

syntax In x α = x ∈ α

inHere : x ∈ (α ▹ x)
inHere = < SExtendL SEmptyL _ >

{-# COMPILE AGDA2HS inHere #-}

inThere : x ∈ α → x ∈ (α ▹ y)
inThere < s > = < SExtendR s _ >

{-# COMPILE AGDA2HS inThere #-}

@0 diff : {α β : Scope name} → α ⊆ β → Scope name
diff = proj₁

-- Bind β variables to the orignal scope α where the body (e.g. a term) may use a subset 
-- of the variables.
record Binder (@0 β : Scope name) (f : @0 Scope name → Set) (@0 α : Scope name) : Set where
  constructor  MkBinder
  field
    @0 {usedScope} : Scope name
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
    cover    : Cover fvₗ fvᵣ α
    outl     : f fvₗ
    outr     : g fvᵣ

{-# COMPILE AGDA2HS Pair deriving Show #-}

open Pair public

data Atom {@0 name : Set} : @0 Scope name → Set where
  None : Atom ∅

{-# COMPILE AGDA2HS Atom deriving Show #-}
