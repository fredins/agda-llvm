
module Formalize.GlobalScope (@0 name  : Set) where

open import Scope

record Globals : Set where
  field
    -- varScope   : Scope name
    defScope   : Scope name
    -- tagScope   : Scope name
    -- fieldScope : All (λ _ → Scope name) tagScope

open Globals public
