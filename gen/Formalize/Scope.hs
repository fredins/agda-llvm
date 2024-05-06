module Formalize.Scope where

data Scope = SEmpty
           | SExtend Scope ()
               deriving Show

instance Semigroup Scope where
    xs <> SEmpty = xs
    xs <> SExtend ys () = SExtend (xs <> ys) ()

instance Monoid Scope where
    mempty = SEmpty
    mappend = (<>)
    mconcat [] = mempty
    mconcat (x : xs) = x <> mconcat xs

cons :: Scope -> Scope
cons xs = SExtend SEmpty () <> xs

rezzExtend :: Scope -> Scope
rezzExtend = \ x -> SExtend x ()

scopeInit :: Scope -> Scope
scopeInit (SExtend xs x) = xs

rezzInit :: Scope -> Scope
rezzInit xs = scopeInit xs

data Cover = CEmptyL
           | CEmptyR
           | CExtendL Cover
           | CExtendR Cover
           | CExtendB Cover
               deriving Show

coverJoinRight :: Scope -> Cover -> Cover
coverJoinRight SEmpty s = s
coverJoinRight (SExtend xs ()) s = CExtendR (coverJoinRight xs s)

data Split = SEmptyL
           | SEmptyR
           | SExtendL Split
           | SExtendR Split
               deriving Show

splitComm :: Split -> Split
splitComm SEmptyL = SEmptyR
splitComm SEmptyR = SEmptyL
splitComm (SExtendL s) = SExtendR (splitComm s)
splitComm (SExtendR s) = SExtendL (splitComm s)

splitAssoc :: Split -> Split -> (Split, Split)
splitAssoc SEmptyL q = (SEmptyL, q)
splitAssoc SEmptyR q = (q, SEmptyL)
splitAssoc p SEmptyR = (p, SEmptyR)
splitAssoc (SExtendL p) (SExtendL q)
  = (SExtendL (fst (splitAssoc p q)), snd (splitAssoc p q))
splitAssoc (SExtendR p) (SExtendL q)
  = (SExtendR (fst (splitAssoc p q)),
     SExtendL (snd (splitAssoc p q)))
splitAssoc p (SExtendR q)
  = (SExtendR (fst (splitAssoc p q)),
     SExtendR (snd (splitAssoc p q)))

splitJoinRight :: Scope -> Split -> Split
splitJoinRight SEmpty s = s
splitJoinRight (SExtend xs ()) s = SExtendR (splitJoinRight xs s)

splitJoinLeft :: Scope -> Split -> Split
splitJoinLeft SEmpty s = s
splitJoinLeft (SExtend xs ()) s = SExtendL (splitJoinLeft xs s)

rezzSplit :: Split -> Scope -> (Scope, Scope)
rezzSplit SEmptyL r = (SEmpty, r)
rezzSplit SEmptyR r = (r, SEmpty)
rezzSplit (SExtendL s) r
  = (rezzExtend (fst (rezzSplit s (rezzInit r))),
     snd (rezzSplit s (rezzInit r)))
rezzSplit (SExtendR s) r
  = (fst (rezzSplit s (rezzInit r)),
     rezzExtend (snd (rezzSplit s (rezzInit r))))

rezzSplitLeft :: Split -> Scope -> Scope
rezzSplitLeft s r = fst (rezzSplit s r)

rezzSplitRight :: Split -> Scope -> Scope
rezzSplitRight s r = snd (rezzSplit s r)

type Split3 = (Split, Split)

type Sub = Split

type In = Sub

inHere :: In
inHere = SExtendL SEmptyL

inThere :: In -> In
inThere s = SExtendR s

data Binder f = MkBinder{usage :: Sub, body :: f}
                  deriving Show

data Pair f g = MkPair{cover :: Cover, outl :: f, outr :: g}
                  deriving Show

data Atom = None
              deriving Show

