
open import Formalize.GlobalScope using (Globals)

module Formalize.Syntax.Grin
  (@0 name    : Set)
  (@0 globals : Globals name)
  where

private open module @0 G = Globals globals

open import Haskell.Prelude renaming (mempty to ∅)
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

-- Names, Val, and Term uses named 'co-de-Bruijn' syntax representation, where 
-- each construction is indexed by its free variables. Compared to de Bruijn indices, 
-- unused variables are discarded at the root (binding site) instead of the leafs 
-- (variable constructors).

data Names : @0 Scope name → Set where
  NNil  : Names ∅
  NCons : (@0 x : name) → Cover (x ◃ ∅) β γ → Names β → Names γ

{-# COMPILE AGDA2HS Names deriving Show #-}

data Val : @0 Scope name → Set where
  Lit : Nat → Val ∅
  Var : (@0 x : name) → Val (x ◃ ∅)

{-# COMPILE AGDA2HS Val deriving Show #-}

data Term : @0 Scope name → Set where
  Return : Val α → Term α
  AppDef : (@0 f : name) → f ∈ defScope → Names α → Term α

{-# COMPILE AGDA2HS Term deriving Show #-}

rezzCover : Cover α β γ → Rezz _ γ
rezzCover CDone      = rezz ∅
rezzCover (CLeft c)  = rezzCong (bind _) (rezzCover c)
rezzCover (CRight c) = rezzCong (bind _) (rezzCover c)
rezzCover (CBoth c)  = rezzCong (bind _) (rezzCover c)

{-# COMPILE AGDA2HS rezzCover #-}

rezzNames : Names α → Rezz _ α
rezzNames NNil         = rezz ∅
rezzNames (NCons x c xs) = rezzCover c

{-# COMPILE AGDA2HS rezzNames #-}

rezzVal : Val α → Rezz _ α
rezzVal (Lit n) = rezz ∅
rezzVal (Var x) = rezz (x ◃ ∅)

{-# COMPILE AGDA2HS rezzVal #-}

rezzTerm : Term α → Rezz _ α
rezzTerm (Return v)      = rezzVal v
rezzTerm (AppDef f p xs) = rezzNames xs

{-# COMPILE AGDA2HS rezzTerm #-}
