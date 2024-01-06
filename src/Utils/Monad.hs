
module Utils.Monad where

foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step pure xs mempty
  where
  step x g acc = g . (acc <>) =<< f x


