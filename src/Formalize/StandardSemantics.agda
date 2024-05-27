{-# OPTIONS --allow-unsolved-metas #-} 

open import Formalize.GlobalScope using (Globals)

module Formalize.StandardSemantics
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Formalize.Syntax.RcGrin name globals
open import Formalize.Scope

data Value : Set where

private variable
  @0 x       : name
  @0 α β γ δ : Scope name

-- Note: 
-- We can implement the interpreter as 'usual' 
-- via recursion and substitution. So, we don't 
-- need a data type for E[e] as in Reiking et al.

eval : Term α → Value
eval (Return (Var n)) = {! !}
eval (AppDef f p ns) = {! !}
eval (bind cover tl r usage tr) = {! !}
eval (Drop n) = {! !}
