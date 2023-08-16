
module Agda.Llvm.Utils where

import           Data.List

import           Agda.TypeChecking.Substitute

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

unionNub :: Eq a => [a] -> [a] -> [a]
unionNub xs = union xs . filter (`notElem` xs)

-- | Non-overloaded version of '\\' (non-associative).
differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
differenceBy eq =  foldl (flip $ deleteBy eq)

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

caseEither :: Either a b -> (a -> c) -> (b -> c) -> c
caseEither e f g = either f g e

-- | This swaps @var 0@ and @var 1@.
swap01' :: Subst a => a -> a
swap01' = applySubst $ deBruijnVar 1 :# liftS 1 (raiseS 1)
