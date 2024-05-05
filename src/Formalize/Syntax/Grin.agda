{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax.Grin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude using (_<>_) 
open import Haskell.Extra.Erase using (Rezz)
open import Agda.Primitive using ()

open import Formalize.Scope using
  ( Scope; ∅; _▹_; Atom; Pair; In; _∈_;
    Binder; Sub; _⊆_
  ) 
open import Formalize.Syntax.Common using ()

private variable
  @0 α β γ : Scope name
  @0 x y   : name

data Name : @0 name → @0 Scope name → Set where
  Only : (@0 x : name) → Name x (∅ ▹ x)

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
  Bind   : Rezz _ β → Pair Term (Binder β Term) α → Term α

{-# COMPILE AGDA2HS Term deriving Show #-}

record Definition : Set where
  constructor MkDef 
  field
    @0 {varsScope} : Scope name
    @0 {freeScope} : Scope name
    vars           : Rezz (Scope name) varsScope
    varsUsage      : freeScope ⊆ varsScope
    term           : Term freeScope

{-# COMPILE AGDA2HS Definition deriving Show #-}

open Definition public

