
------------------------------------------------------------------------
-- Examples of the Perceus algorithm.

module Formalize.Test where

open import Agda.Builtin.String using (String)
open import Haskell.Prelude using (_≡_; refl)
open import Haskell.Extra.Erase using (<_>; rezz)
open import Haskell.Extra.Refinement using (value)

-- Defines everything that is globally known.
open import Formalize.GlobalScope using ()

-- Scoping library.
open import Formalize.Scope

-- We use strings as names for readability, but this doesn't
-- matter since names are erased.

name = String

-- We define the global function names and variable names.
-- GRIN's interprocedural heap point analysis determines the tags
-- every variable can be assigned to, so all the variables are
-- globally known.

globals = record
  { defScope = ∅ ▹ "f" ▹ "g"
  }

-- Grin syntax before the Perceus algorithm (no dup/drop).
open import Formalize.Syntax.Grin name globals

-- Grin syntax with dup and drop.
import Formalize.Syntax.RcGrin name globals as R

-- The syntax-directed rules and implementation.
open import Formalize.MiniGrin name globals

-- The first example is a function call that binds two variables, y and z,
-- but only uses y. We assume that all variables are heap allocated and thus
-- reference counted. Hence, we want the Perceus algorithm to drop the z variable
-- a soon as possible.

-- f0 x =
--   f x ; λ y z →
--   return y
f0 : Definition
f0 = record
  { vars = rezz (∅ ▹ "x")
  ; varsUsage = < SEmptyR >
  ; term =
    bind 
      CEmptyR
      (AppDef "f" (inThere inHere) (cons CEmptyR (Only "x") nil))
      (rezz (∅ ▹ "y" ▹ "z"))
      < SExtendR SEmptyR "z" >
      (Return (var "y"))
  }

{-# COMPILE AGDA2HS f0 #-}

-- Following is the expected result f0′ and a test that the algorithm produces the
-- expected result.

-- f0′ x =
--   f x ; λ y z →
--   drop z ;
--   return y
f0′ : R.Definition
f0′ = record
  { vars = rezz (∅ ▹ "x")
  ; term =
    R.bind  
     CEmptyR
     (R.AppDef "f" (inThere inHere) (R.cons CEmptyR (R.Only "x") R.nil))
     (rezz (∅ ▹ "y" ▹ "z"))
     < SEmptyR > 
     (R.bind 
       (CExtendL CEmptyL "z") 
       (R.drop "z") 
       (rezz ∅) 
       < SEmptyR > 
       (R.Return (R.var "y")))
  }


-- We only care about the existence of a proof (and not its structure), so we project the value
-- part of the output, and compare it to our expected result.

-- f0 x =
--   f x ; λ y z →
--   return y
-- >>>
-- f0′ x =
--   f x ; λ y z →
--   drop z ;
--   return y
_ : value (perceus f0) ≡ f0′
_ = refl

-- Our second example has both unused variables (y, b, and z) and the shared variable x.

-- f1 x y =
--   f x ; λ z a b →
--   g a x
f1 : Definition
f1 = record
  { vars = rezz (∅ ▹ "x" ▹ "y")
  ; varsUsage = < SExtendR SEmptyR "y" >
  ; term =
    bind 
      (CExtendB CEmptyL "x")
      (AppDef "f" (inThere inHere) (cons CEmptyR (Only "x") nil))
      (rezz (∅ ▹ "z" ▹ "a" ▹ "b"))
      < SExtendR (SExtendL SEmptyL "a") "b" >
      (AppDef "g" inHere (cons (CExtendL CEmptyL "a") (Only "a") (cons CEmptyR (Only "x") nil)))
  }

{-# COMPILE AGDA2HS f1 #-}

-- We want the algorithm to dup x in the first usage, and as late as possible.
-- Unlike drop, dup can appear at every place variables can appear.

-- f1′ x y =
--   drop y ;
--   f (dup x) ; λ z a b →
--   drop b ;
--   drop z ;
--   g a x
f1′ : R.Definition
f1′ = record
  { vars = rezz (∅ ▹ "x" ▹ "y")
  ; term =
    R.bind 
      (CExtendL CEmptyL "y")
      (R.drop "y") 
      (rezz ∅)
      < SEmptyR >
       (R.bind 
         (CExtendB CEmptyL "x")
         (R.AppDef "f" (inThere inHere) (R.cons CEmptyR (R.Dup "x") R.nil))
         (rezz (∅ ▹ "z" ▹ "a" ▹ "b"))
         < SEmptyR >
           (R.bind 
             (CExtendL CEmptyL "b")
             (R.drop "b")
             (rezz ∅)
             < SEmptyR >
             (R.bind 
               (CExtendR (CExtendL CEmptyL "z") "a") 
               (R.drop "z") 
               (rezz ∅) 
               < SEmptyR >
               (R.AppDef "g" inHere (R.cons (CExtendL CEmptyL "a") (R.Only "a") (R.cons CEmptyR (R.Only "x") R.nil))))))
  }

-- f1 x y =
--   f x ; λ z a b →
--   g a x
-- >>>
-- f1′ x y =
--   drop y ;
--   f (dup x) ; λ z a b →
--   drop b ;
--   drop z ;
--   g a x
_ : value (perceus f1) ≡ f1′
_ = refl
