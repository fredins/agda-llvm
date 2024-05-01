{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax.RcGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (_<>_) 
open import Haskell.Extra.Erase 
open import Haskell.Law.Equality 
open import Haskell.Law.Monoid.Def using (leftIdentity; rightIdentity)
open import Agda.Primitive
open import Haskell.Law.Semigroup.Def

open import Formalize.Scope
open import Formalize.Utils.Erase
open import Formalize.Syntax.Common

private variable
  @0 α β γ δ : Scope name
  @0 x y     : name

data Name : @0 name → @0 Scope name → Set where
  Only : (@0 x : name) → Name x (∅ ▹ x)
  Dup  : (@0 x : name) → Name x (∅ ▹ x)

{-# COMPILE AGDA2HS Name deriving Show #-}

data Names (@0 α : Scope name) : Set where
  NNil : Atom α → Names α
  NCons : Pair (Name x) Names α → Names α

{-# COMPILE AGDA2HS Names deriving Show #-}

data Val (@0 α : Scope name) : Set where
  Var : Name x α → Val α

{-# COMPILE AGDA2HS Val deriving Show #-}

data Term (@0 α : Scope name) : Set where
  Return : Val α → Term α
  AppDef : (@0 f : name) → f ∈ defScope → Names α → Term α
  Bind   : Rezz (Scope name) β → Pair Term (Binder β Term) α → Term α
  Drop   : Name x α → Term α

{-# COMPILE AGDA2HS Term deriving Show #-}

-- TODO: add varsUsage once primitives are added.
record Definition : Set where
  constructor MkDef
  field
    @0 {varsScope} : Scope name
    vars           : Rezz (Scope name) varsScope
    term           : Term varsScope

{-# COMPILE AGDA2HS Definition deriving Show #-}

open Definition public

-- TODO: can we make this a pattern synonym?
bindEmpty : Cover α β γ → Term α → Term β → Term γ
bindEmpty c tl tr = Bind (rezz ∅) (MkPair c tl (MkBinder < SEmptyR > (subst0 Term (sym (rightIdentity _)) tr)))

{-# COMPILE AGDA2HS bindEmpty #-}

drop : (@0 x : name) → Term (∅ ▹ x)
drop x = Drop (Only x) 

{-# COMPILE AGDA2HS drop #-}

dropsAux : Rezz (Scope name) δ → α ⋈ δ ≡ β → Term (γ <> α) → Term (γ <> β)
dropsAux {γ = _} (rezz ∅)       SEmptyL        t = t
dropsAux {γ = _} (rezz ∅)       SEmptyR        t = t
dropsAux {γ = _} (rezz ∅)       (SExtendL s x) t = subst0 _ (∅-⋈-injective (splitComm s)) t
dropsAux {γ = γ} (rezz (xs ▹ x)) SEmptyL        t = 
  subst0 Term (<>-▹-assoc γ xs x) 
    (bindEmpty (CExtendL CEmptyL x) (drop x) (dropsAux (rezz xs) SEmptyL t))
dropsAux {γ = γ} (rezz (xs ▹ x)) (SExtendL s y) t = go (∅ ▹ y) s t 
  where 
  go : ∀ {@0 α β} → (ys : Scope name) → α ⋈ (xs ▹ x) ≡ β → Term (γ <> α <> ys) → Term (γ <> β <> ys) 
  go {β = β} ys SEmptyL t = 
    subst0 Term (sym (associativity γ (xs ▹ x) ys)) 
     (subst0 (λ γ  → Term (γ <> ys)) (sym (<>-▹-assoc γ xs x)) 
       (bindEmpty (coverJoinRight (rezz ys) (CExtendL CEmptyL x)) (drop x) 
         (subst0 Term (associativity γ xs ys) 
           (dropsAux (rezz xs) (splitJoinLeft (rezz ys) SEmptyL) t))))
  go {α = α ▹ y′} {β = β ▹ y′} (ys) (SExtendL {γ = β} s y′) t = 
    subst0 (λ β → Term (γ <> β)) (sym (▹-<>≡<>-◃ β y′ ys)) 
      (go ((y′ ◃ ys)) s (subst0 (λ β → Term (γ <> β)) (▹-<>≡<>-◃ α y′ ys) t))
  go ys (SExtendR {γ = β} s x) t = 
    subst0 Term (sym (associativity γ (β ▹ x) ys))
      (bindEmpty (coverJoinRight {β = γ <> β} (rezz ys) (CExtendL CEmptyL x)) (drop x) 
        (subst0 Term (associativity γ β ys) (dropsAux (rezz xs) (splitJoinLeft (rezz ys) s) t)))
dropsAux {γ = γ} (rezz (xs ▹ x)) (SExtendR s x) t = 
  subst0 Term (sym (<>-▹-assoc γ _ x)) 
    (bindEmpty (CExtendL CEmptyL x) (drop x) (dropsAux (rezz xs) s t)) 

{-# COMPILE AGDA2HS dropsAux #-}

drops : Rezz (Scope name) β → α ⊆ β → Term (γ <> α) → Term (γ <> β)
drops r < s > = dropsAux (rezzSplitRight s r) s

{-# COMPILE AGDA2HS drops #-}

drops' : Rezz (Scope name) β → α ⊆ β → Term α → Term β
drops' r p t = subst0 Term (leftIdentity _) (drops r p (subst0 Term (sym (leftIdentity _)) t)) 

{-# COMPILE AGDA2HS drops' #-}




