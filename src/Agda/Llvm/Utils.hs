
module Agda.Llvm.Utils
  ((.:)
  , unionNub
  , differenceBy
  , (??)
  , caseEither
  , swap01'
  , printPretty
  , trace'
  , forAccumR
  , mapAccumM
  , forAccumM
  ) where

import           Control.Monad                (liftM)
import           Data.Coerce                  (Coercible, coerce)
import           Data.Functor                 (($>))
import           Data.List                    (deleteBy, mapAccumR, union)
import           GHC.IO                       (unsafePerformIO)

import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.Substitute

infixr 8 .:
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

printPretty :: Pretty a => a -> IO ()
printPretty = putStrLn . prettyShow

trace' :: String -> a -> a
trace' s = unsafePerformIO . (appendFile "trace.log" (s ++ "\n") $>)

forAccumR :: Traversable t => s -> t a -> (s -> a -> (s, b)) -> (s, t b)
forAccumR s t f = mapAccumR f s t

-----------------------------------------------------------------------
-- * from base 4.18.0.0
-----------------------------------------------------------------------

mapAccumM
  :: forall m t s a b. (Monad m, Traversable t)
  => (s -> a -> m (s, b))
  -> s -> t a -> m (s, t b)
mapAccumM f s t = coerce (mapM @t @(StateT s m) @a @b) (StateT #. flip f) t s

forAccumM
  :: (Monad m, Traversable t)
  => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
{-# INLINE forAccumM #-}
forAccumM s t f = mapAccumM f s t

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

instance Monad m => Functor (StateT s m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
