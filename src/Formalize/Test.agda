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

import Formalize.MiniGrin name globals as M
import Formalize.Syntax.Grin name globals as G
import Formalize.Syntax.RcGrin name globals as R

ex0 : G.Term ("x" ◃ ∅)
ex0 = G.Return (G.Var "x")
  
{-# COMPILE AGDA2HS ex0 #-}

ex0' : R.Term ("x" ◃ ∅)
ex0' = R.Return (R.Var (R.NoDup "x"))

{-# COMPILE AGDA2HS ex0' #-}

test0 = M.perceusTerm ex0 splitEmptyLeft

{-# COMPILE AGDA2HS test0 #-}

ex1 : G.Term ("x" ◃ "y" ◃ ∅)
ex1 = G.AppDef "f" inHere (G.NCons "x" (both (right done)) (G.NCons "y" (right (left done)) (G.NCons "x" (left done) G.NNil)))

{-# COMPILE AGDA2HS ex1 #-}

ex1' : R.Term ("x" ◃ "y" ◃ ∅)
ex1' = R.AppDef "f" inHere (R.NCons (R.Dup "x") (both (right done)) (R.NCons (R.NoDup "y") (right (left done)) (R.NCons (R.NoDup "x") (left done) R.NNil)))

{-# COMPILE AGDA2HS ex1' #-}

test1 = M.perceusTerm ex1 splitEmptyLeft

{-# COMPILE AGDA2HS test1 #-}
