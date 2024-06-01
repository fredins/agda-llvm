{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax.RcGrin2
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (_<>_) 
open import Haskell.Extra.Erase 
open import Haskell.Extra.Sigma
open import Haskell.Law.Equality 
open import Haskell.Law.Monoid.Def using (leftIdentity; rightIdentity)
open import Agda.Primitive
open import Haskell.Law.Semigroup.Def

open import Formalize.Scope2
open import Formalize.Utils.Erase
open import Formalize.Syntax.Common

private variable
  @0 α β γ δ : Scope name
  @0 x y     : name

-- Note: 
--   It is perhaps better to have Dup as a Term constructor instead, 
--   in the form: "dup x ; λ x′ → ‥". However, at the moment I am 
--   not fully convinced it would be better. It would require us to 
--   implement subsitution, and it may complicate the proof of precision. 
data Name : @0 name → @0 Scope name → Set where
  Only : (@0 x : name) → Name x (∅ ▹ x)
  Dup  : (@0 x : name) → Name x (∅ ▹ x)

{-# COMPILE AGDA2HS Name deriving Show #-}

data Names (@0 α : Scope name) : Set where
  NNil : Atom α → Names α
  NCons : Pair (Name x) Names α → Names α

{-# COMPILE AGDA2HS Names deriving Show #-}

pattern cons cover x xs = NCons (MkPair cover x xs)
pattern nil = NNil None

data Val (@0 α : Scope name) : Set where
  Var : Name x α → Val α

{-# COMPILE AGDA2HS Val deriving Show #-}

pattern var x = Var (Only x)

data Term (@0 α : Scope name) : Set where
  Return : Val α → Term α
  -- TODO replace Names with Vals for easier substitution.
  AppDef : (@0 f : name) → f ∈ defScope → Names α → Term α
  Bind   : Rezz (Scope name) β → Pair Term (Binder β Term) α → Term α
  Drop   : Name x α → Term α

  -- An alternative is to have Subst constructor:
  -- Subst  : Term α → (name → Val α) → Term α

{-# COMPILE AGDA2HS Term deriving Show #-}

pattern bind cover tl r usage tr = Bind r (MkPair cover tl (MkBinder usage tr))
pattern drop x = Drop (Only x) 

rezzName : Name x α → Rezz (Scope name) α
rezzName (Only x) = rezz (∅ ▹ x)
rezzName (Dup x) = rezz (∅ ▹ x)

{-# COMPILE AGDA2HS rezzName #-}

rezzNames : Names α → Rezz (Scope name) α
rezzNames nil = rezz ∅
rezzNames (cons cover n ns) = rezzCover cover

{-# COMPILE AGDA2HS rezzNames #-}

rezzVal : Val α → Rezz (Scope name) α
rezzVal (Var n) = rezzName n

{-# COMPILE AGDA2HS rezzVal #-}

rezzTerm : Term α → Rezz (Scope name) α
rezzTerm (Return v) = rezzVal v
rezzTerm (AppDef f p ns) = rezzNames ns
rezzTerm (bind cover tl r usage tr) = rezzCover cover
rezzTerm (Drop n) = rezzName n

{-# COMPILE AGDA2HS rezzTerm #-}

-- TODO: add varsUsage once primitives are added.
record Definition : Set where
  constructor MkDef
  field
    @0 {varsScope} : Scope name
    vars           : Rezz (Scope name) varsScope
    term           : Term varsScope

{-# COMPILE AGDA2HS Definition deriving Show #-}

open Definition public

bindEmpty : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Cover Φ Θ → Term α → Term β → Term γ
bindEmpty cover tl tr = bind cover tl (rezz ∅) empty (subst0 Term (sym (rightIdentity _)) tr)

{-# COMPILE AGDA2HS bindEmpty #-}

dropsAux : {@0 Φ : α ⊆ γ} {@0 Θ : β ⊆ γ} → Rezz (Scope name) δ → Split Φ Θ → Term (α <> δ) → Term (γ <> δ)
dropsAux r empty t = t
dropsAux {α = α ▹ x} {γ = γ ▹ x} {δ = δ} r (s left x) t = 
  subst0 Term (sym (▹-<>≡<>-◃ γ x δ)) 
    (dropsAux (rezzCong (x ◃_) r) s (subst0 Term (▹-<>≡<>-◃ α x δ) t))
dropsAux {α = α} {γ = γ ▹ x} {β = β ▹ x} {δ = δ} r (s right x) t = 
  bindEmpty (coverEmptyLeft (rezzSplit s) left x rights r) (drop x) 
    (dropsAux r s t)

{-# COMPILE AGDA2HS dropsAux #-}

drops : α ⊆ β → Term (γ <> α) → Term (γ <> β)
drops {α = α} {β = β} {γ = γ} p t = 
  let 
    r = rezzAppendLeft (rezzTerm t) (rezzSubLeft p)
    < q , s > = subToSplit p
  in 
    dropsAux (rezz ∅) (splitAppend (splitEmptyRight r) s) t

{-# COMPILE AGDA2HS drops #-}
