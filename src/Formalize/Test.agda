{-# OPTIONS --allow-unsolved-metas #-} 

module Formalize.Test where

open import Agda.Builtin.String using (String)
open import Haskell.Prelude using (_≡_; refl) renaming (mempty to ∅)
open import Haskell.Extra.Refinement 

open import Scope
open import Formalize.GlobalScope
open import Formalize.Scope.Base

name = String
globals = record{defScope = "f" ◃ ∅}

open import Formalize.MiniGrin name globals
open import Formalize.Syntax.Grin name globals
import Formalize.Syntax.RcGrin name globals as Rc

ex₀ : Term ("x" ◃ ∅)
ex₀ = Return (Var "x")
  
ex₀′ : Rc.Term ("x" ◃ ∅)
ex₀′ = Rc.Return (Rc.Var (Rc.NoDup "x"))

test₀ = perceusTerm ex₀ splitEmptyLeft

ex₁ : Term ("x" ◃ "y" ◃ ∅)
ex₁ = AppDef "f" inHere (NCons "x" (both (right done)) (NCons "y" (right (left done)) (NCons "x" (left done) NNil)))

ex₁′ : Rc.Term ("x" ◃ "y" ◃ ∅)
ex₁′ = Rc.AppDef "f" inHere (Rc.NCons (Rc.Dup "x") (both (right done)) (Rc.NCons (Rc.NoDup "y") (right (left done)) (Rc.NCons (Rc.NoDup "x") (left done) Rc.NNil)))

test₁ = perceusTerm ex₁ splitEmptyLeft
