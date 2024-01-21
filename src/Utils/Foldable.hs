
module Utils.Foldable where

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap
