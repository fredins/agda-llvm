
module Utils.List where

import           Control.Monad   (void)
import           Data.List       (deleteBy, union, zipWith4)
import           Data.List.Extra (foldl')

import           Agda.Utils.List

unionNub :: Eq a => [a] -> [a] -> [a]
unionNub xs = union xs . filter (`notElem` xs)

-- | Non-overloaded version of '\\' (non-associative).
differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
differenceBy eq =  foldl' (flip $ deleteBy eq)

forMaybeAndRest :: [a] -> (a -> Maybe b) -> ([b], Suffix a)
forMaybeAndRest = flip mapMaybeAndRest

zipWith3M :: Applicative f => (a -> b -> c -> f d) -> [a] -> [b] -> [c] -> f [d]
zipWith3M f xs ys zs = sequenceA $ zipWith3 f xs ys zs

zipWith3M_ :: Applicative f => (a -> b -> c -> f d) -> [a] -> [b] -> [c] -> f ()
zipWith3M_ f xs ys zs = void $ zipWith3M f xs ys zs

zipWith4M :: Applicative f => (a -> b -> c -> d -> f e) -> [a] -> [b] -> [c] -> [d] -> f [e]
zipWith4M f xs ys zs as = sequenceA $ zipWith4 f xs ys zs as

zipWith4M_ :: Applicative f => (a -> b -> c -> d -> f e) -> [a] -> [b] -> [c] -> [d] -> f ()
zipWith4M_ f xs ys zs as = void $ zipWith4M f xs ys zs as



