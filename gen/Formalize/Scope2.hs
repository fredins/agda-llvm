module Formalize.Scope2 where

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

prepend :: Scope -> Scope
prepend xs = SExtend SEmpty () <> xs

rezzExtend :: Scope -> Scope
rezzExtend = \ x -> SExtend x ()

scopeInit :: Scope -> Scope
scopeInit (SExtend xs x) = xs

rezzInit :: Scope -> Scope
rezzInit xs = scopeInit xs

data Thinning = Empty
              | Keep Thinning
              | Discard Thinning

data Cover = CEmpty
           | CLeft Cover
           | CRight Cover
           | CBoth Cover

