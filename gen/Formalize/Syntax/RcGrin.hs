module Formalize.Syntax.RcGrin where

import Formalize.Scope (Atom, Binder(MkBinder), Cover(CEmptyL, CExtendL), In, Pair(MkPair), Scope(SEmpty, SExtend), Split(SEmptyL, SEmptyR, SExtendL, SExtendR), Sub, cons, coverJoinRight, rezzSplitRight, splitJoinLeft)

data Name = Only
          | Dup
              deriving Show

data Names = NNil Atom
           | NCons (Pair Name Names)
               deriving Show

data Val = Var Name
             deriving Show

data Term = Return Val
          | AppDef In Names
          | Bind Scope (Pair Term (Binder Term))
          | Drop Name
              deriving Show

data Definition = MkDef{vars :: Scope, term :: Term}
                    deriving Show

bindEmpty :: Cover -> Term -> Term -> Term
bindEmpty c tl tr = Bind SEmpty (MkPair c tl (MkBinder SEmptyR tr))

drop :: Term
drop = Drop Only

dropsAux :: Scope -> Split -> Term -> Term
dropsAux SEmpty SEmptyL t = t
dropsAux SEmpty SEmptyR t = t
dropsAux SEmpty (SExtendL s) t = t
dropsAux (SExtend xs ()) SEmptyL t
  = bindEmpty (CExtendL CEmptyL) (Drop Only) (dropsAux xs SEmptyL t)
dropsAux (SExtend xs ()) (SExtendL s) t
  = go (SExtend SEmpty ()) s t
  where
    go :: Scope -> Split -> Term -> Term
    go ys SEmptyL t
      = bindEmpty (coverJoinRight ys (CExtendL CEmptyL)) (Drop Only)
          (dropsAux xs (splitJoinLeft ys SEmptyL) t)
    go ys (SExtendL s) t = go (cons ys) s t
    go ys (SExtendR s) t
      = bindEmpty (coverJoinRight ys (CExtendL CEmptyL)) (Drop Only)
          (dropsAux xs (splitJoinLeft ys s) t)
dropsAux (SExtend xs ()) (SExtendR s) t
  = bindEmpty (CExtendL CEmptyL) (Drop Only) (dropsAux xs s t)

drops :: Scope -> Sub -> Term -> Term
drops r s = dropsAux (rezzSplitRight s r) s

drops' :: Scope -> Sub -> Term -> Term
drops' r p t = drops r p t

