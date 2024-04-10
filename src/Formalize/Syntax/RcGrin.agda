
open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax.RcGrin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (Nat) renaming (mempty to ∅)
open import Haskell.Prim using (it)
open import Haskell.Extra.Erase 
open import Haskell.Law

open import Scope 
open import Formalize.Scope

postulate 
  Tag : Set

private variable
  @0 x     : name
  @0 α β γ : Scope name

data Name : @0 name → Set where
  NoDup Dup : (@0 x : name) → Name x

data Names : @0 Scope name → Set where
  NNil  : Names ∅
  NCons : Name x → Cover (x ◃ ∅) β γ → Names β → Names γ

data Val : @0 Scope name → Set where
  Lit : Nat → Val ∅
  Var : Name x → Val (x ◃ ∅)

data Term : @0 Scope name → Set where
  Return : Val α → Term α
  AppDef : (@0 f : name) → f ∈ defScope → Names α → Term α
