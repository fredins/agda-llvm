
------------------------------------------------------------------------
-- Examples of the Perceus algorithm.

module Formalize.Test where

open import Agda.Builtin.String using (String)
open import Haskell.Prelude using (_≡_; refl) 
open import Haskell.Extra.Erase 
open import Haskell.Extra.Refinement 

open import Formalize.GlobalScope
open import Formalize.Scope

-- We use strings as names for readability, but this doesn't 
-- matter since names are erased.

name = String

-- We define the global function names and variable names. 
-- GRIN's interproderal heap point analysis determines the tags 
-- every variable can be assigned to, so all the variables are 
-- globally known. 

globals = record
  { defScope = ∅ ▹ "f" ▹ "g"
  ; varScope = ∅ ▹ "x" ▹ "y" ▹ "z" ▹ "a" ▹ "b" ▹ "c"
  }

open import Formalize.Syntax.Grin name globals
import Formalize.Syntax.RcGrin name globals as R
open import Formalize.MiniGrin name globals 

-- The first example is a function call that binds two variables, y and z,
-- but only uses y. We assume that all variables are heap allocated and thus 
-- reference counted. Hence, we want the Perceus algorithm to drop the z variable 
-- a soon as possible.

-- f0 x = f x ; λ y z → return y
f0 : Definition
f0 = record
  { vars = rezz (∅ ▹ "x")
  ; varsUsage = < SEmptyR >
  ; term = 
    Bind (rezz (∅ ▹ "y" ▹ "z")) (MkPair CEmptyR 
     (AppDef "f" (inThere inHere) (NCons (MkPair CEmptyR (Only "x") (NNil None)))) 
     (MkBinder < SExtendR SEmptyR "z" > (Return (Var (Only "y")))))
  }

{-# COMPILE AGDA2HS f0 #-}

-- Following is the expected result f0′ and a test that the algorithm produces the 
-- expected result.

-- f0′ x = f x ; λ y z → drop z; return y
f0′ : R.Definition
f0′ = record
  { vars = rezz (∅ ▹ "x")
  ; term = 
    R.Bind (rezz (∅ ▹ "y" ▹ "z")) (MkPair CEmptyR
     (R.AppDef "f" (inThere inHere) (R.NCons (MkPair CEmptyR (R.Only "x") (R.NNil None))))
     (MkBinder < SEmptyR > (R.Bind (rezz ∅) (MkPair (CExtendL CEmptyL "z") 
       (R.Drop (R.Only "z"))
        (MkBinder < SEmptyR >
         (R.Return (R.Var (R.Only "y"))))))))
  }


-- We only care about the existence of a proof (and not its structure), so we project the value 
-- part of the output, and compare it to our expected result. 

-- f0 x = f x ; λ y z → return y
-- >>>
-- f0 x = f x ; λ y z → drop z; return y
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
    Bind (rezz (∅ ▹ "z" ▹ "a" ▹ "b")) (MkPair (CExtendB CEmptyL "x") 
      (AppDef "f" (inThere inHere) (NCons (MkPair CEmptyR (Only "x") (NNil None)))) (MkBinder < SExtendR (SExtendL SEmptyL "a") "b" > 
      (AppDef "g" inHere (NCons (MkPair (CExtendL CEmptyL "a") (Only "a") (NCons (MkPair CEmptyR (Only "x") (NNil None))))))))
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
    R.Bind (Rezzed ∅ refl)
      (MkPair (CExtendL CEmptyL "y")
       (R.Drop
        (R.Only "y"))
       (MkBinder < SEmptyR >
        (R.Bind
         (rezz (∅ ▹ "z" ▹ "a" ▹ "b"))
         (MkPair (CExtendB CEmptyL "x")
          (R.AppDef "f" (inThere inHere)
           (R.NCons
            (MkPair CEmptyR (R.Dup "x")
             (R.NNil None))))
          (MkBinder < SEmptyR >
           (R.Bind (rezz ∅)
            (MkPair (CExtendL CEmptyL "b")
             (R.Drop
              (R.Only "b"))
             (MkBinder < SEmptyR > 
              (R.Bind (rezz ∅)
               (MkPair (CExtendR (CExtendL CEmptyL "z") "a")
                (R.Drop
                 (R.Only "z"))
                (MkBinder < SEmptyR >
                 (R.AppDef "g" inHere
                  (R.NCons
                   (MkPair (CExtendL CEmptyL "a")
                    (R.Only "a")
                    (R.NCons
                     (MkPair CEmptyR (R.Only "x")
                      (R.NNil None)))))))))))))))))

  }

_ : value (perceus f1) ≡ f1′
_ = refl
