
module Utils.List where

import           Data.List       (deleteBy, union)

import           Agda.Utils.List

unionNub :: Eq a => [a] -> [a] -> [a]
unionNub xs = union xs . filter (`notElem` xs)

-- | Non-overloaded version of '\\' (non-associative).
differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
differenceBy eq =  foldl (flip $ deleteBy eq)

forMaybeAndRest :: [a] -> (a -> Maybe b) -> ([b], Suffix a)
forMaybeAndRest = flip mapMaybeAndRest

zipWith3M :: Applicative f => (a -> b -> c -> f d) -> [a] -> [b] -> [c] -> f [d]
zipWith3M f xs ys zs = sequenceA $ zipWith3 f xs ys zs
