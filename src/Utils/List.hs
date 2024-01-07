
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
