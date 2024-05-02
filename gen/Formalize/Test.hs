module Formalize.Test where

import Formalize.Scope (Atom(None), Binder(MkBinder), Cover(CEmptyL, CEmptyR, CExtendB, CExtendL), Pair(MkPair), Scope(SEmpty, SExtend), Split(SEmptyL, SEmptyR, SExtendL, SExtendR), inHere, inThere)
import qualified Formalize.Syntax.Grin (Definition(MkDef), Name(Only), Names(NCons, NNil), Term(AppDef, Bind, Return), Val(Var))

f0 :: Formalize.Syntax.Grin.Definition
f0
  = Formalize.Syntax.Grin.MkDef (SExtend SEmpty ()) SEmptyR
      (Formalize.Syntax.Grin.Bind (SExtend (SExtend SEmpty ()) ())
         (MkPair CEmptyR
            (Formalize.Syntax.Grin.AppDef (inThere inHere)
               (Formalize.Syntax.Grin.NCons
                  (MkPair CEmptyR Formalize.Syntax.Grin.Only
                     (Formalize.Syntax.Grin.NNil None))))
            (MkBinder (SExtendR SEmptyR)
               (Formalize.Syntax.Grin.Return
                  (Formalize.Syntax.Grin.Var Formalize.Syntax.Grin.Only)))))

f1 :: Formalize.Syntax.Grin.Definition
f1
  = Formalize.Syntax.Grin.MkDef (SExtend (SExtend SEmpty ()) ())
      (SExtendR SEmptyR)
      (Formalize.Syntax.Grin.Bind
         (SExtend (SExtend (SExtend SEmpty ()) ()) ())
         (MkPair (CExtendB CEmptyL)
            (Formalize.Syntax.Grin.AppDef (inThere inHere)
               (Formalize.Syntax.Grin.NCons
                  (MkPair CEmptyR Formalize.Syntax.Grin.Only
                     (Formalize.Syntax.Grin.NNil None))))
            (MkBinder (SExtendR (SExtendL SEmptyL))
               (Formalize.Syntax.Grin.AppDef inHere
                  (Formalize.Syntax.Grin.NCons
                     (MkPair (CExtendL CEmptyL) Formalize.Syntax.Grin.Only
                        (Formalize.Syntax.Grin.NCons
                           (MkPair CEmptyR Formalize.Syntax.Grin.Only
                              (Formalize.Syntax.Grin.NNil None)))))))))

