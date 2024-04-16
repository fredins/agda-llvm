module Formalize.Experiment where

open import Agda.Builtin.String using (String)
open import Haskell.Prelude using (_≡_; refl) renaming (mempty to ∅)

open import Scope
open import Formalize.GlobalScope
open import Formalize.Utils.Erase 

name = String
globals = record{defScope = "f" ◃ ∅; varScope = "x" ◃ "y" ◃ "z" ◃ ∅}


private variable
  @0 α β γ : Scope name
  @0 x y   : name
  l : α ⊆ γ
  r : β ⊆ γ

record Pair (f : @0 Scope name → Set) (@0 scope : Scope name) : Set where
  constructor MkPair
  field 
    @0 {support} : Scope name
    thinning  : support ⊆ scope
    thing     : f support

{-# COMPILE AGDA2HS Pair deriving Show #-}

open Pair public 

pattern _↑_ x y = MkPair x y

data Cover : @0 α ⊆ γ → @0 β ⊆ γ → Set where
  CDone  : Cover {γ = ∅} subEmpty subEmpty
  CLeft  : Cover l r → Cover (subBindKeep {y = y} l) (subBindDrop r)
  CRight : Cover l r → Cover (subBindDrop {y = y} l) (subBindKeep r) 
  CBoth  : Cover l r → Cover (subBindKeep {y = y} l) (subBindKeep r)

{-# COMPILE AGDA2HS Cover deriving Show #-}

record Fork (f g : @0 Scope name → Set) (@0 α : Scope name) : Set where
  constructor MkFork 
  field 
    @0 {supportl} : Scope name
    @0 {supportr} : Scope name
    {thinningl}   : supportl ⊆ α
    {thinningr}   : supportr ⊆ α
    cover         : Cover thinningl thinningr
    outl          : f supportl
    outr          : g supportr

{-# COMPILE AGDA2HS Fork deriving Show #-}

open Fork public

data Name (@0 x : name) : @0 Scope name → Set where
  Only : Name x (x ◃ ∅)

{-# COMPILE AGDA2HS Name deriving Show #-}

data Tm (@0 α : Scope name) : Set where
  Var : (@0 x : name) → Name x α → Tm α
  Lam : (@0 x : name) → Pair Tm (x ◃ α) → Tm α
  App : Fork Tm Tm α → Tm α

{-# COMPILE AGDA2HS Tm deriving Show #-}
  
ex0 : Tm ∅ 
ex0 = Lam "f" (subRefl ↑ Lam "x" (subRefl ↑ App (MkFork (CRight (CLeft CDone)) (Var "f" Only) (Var "x" Only))))

{-# COMPILE AGDA2HS ex0 #-}

