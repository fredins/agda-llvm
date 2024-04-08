
open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude renaming (mempty to ∅)
open import Haskell.Extra.Erase 

open import Scope 

postulate 
  Tag : Set

private variable
  @0 α β γ : Scope name

-- From "Everybody’s Got To Be Somewhere" by Connor McBride, 2018. Also, see 
-- Jesper Cockx's blogpost for a short introduction: 
-- https://jesper.sikanda.be/posts/1001-syntax-representations.html.
data Cover : (@0 α β γ : Scope name) → Set where
  CDone  : Cover ∅ ∅ ∅
  CLeft  : (@0 x : name) → Cover α β γ → Cover (x ◃ α) β       (x ◃ γ)
  CRight : (@0 x : name) → Cover α β γ → Cover α       (x ◃ β) (x ◃ γ)
  CBoth  : (@0 x : name) → Cover α β γ → Cover (x ◃ α) (x ◃ β) (x ◃ γ)

-- Args, Val, and Term uses named 'co-de-Bruijn' syntax representation, where 
-- each construction is indexed by its free variables. Compared to de Bruijn indices, 
-- unused variables are discarded at the root (binding site) instead of the leafs 
-- (variable constructors).

data Args : @0 Scope name → Set where
  ArgsNil  : Args ∅
  ArgsCons : (@0 x : name) → Cover (x ◃ ∅) α β → Args α → Args β

data Val : @0 Scope name → Set where
  Lit : Nat → Val ∅
  Var : (@0 x : name) → Val (x ◃ ∅)

data Term : @0 Scope name → Set where
  Return : Val α → Term α
  AppDef : (@0 f : name) → f ∈ defScope → Args α → Term α

-- Free variables of Cover, Args, Val, and Term.

rezzCover : Cover α β γ → Rezz _ γ
rezzCover CDone        = rezz ∅
rezzCover (CLeft x c)  = rezzCong (bind x) (rezzCover c)
rezzCover (CRight x c) = rezzCong (bind x) (rezzCover c)
rezzCover (CBoth x c)  = rezzCong (bind x) (rezzCover c)

rezzArgs : Args α → Rezz _ α
rezzArgs ArgsNil           = rezz ∅
rezzArgs (ArgsCons x c xs) = rezzCover c

rezzVal : Val α → Rezz _ α
rezzVal (Lit n) = rezz ∅
rezzVal (Var x) = rezz (x ◃ ∅)

rezzTerm : Term α → Rezz _ α
rezzTerm (Return v)      = rezzVal v
rezzTerm (AppDef f p xs) = rezzArgs xs

