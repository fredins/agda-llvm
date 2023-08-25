
module Agda.Llvm.Utils
  ((.:)
  , list1zip3
  , list1zipWith3
  , list1unzip4
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
import           Data.List                    (deleteBy, mapAccumR, union,
                                               unzip4)
import           GHC.IO                       (unsafePerformIO)

import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.Substitute
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

list1zip3 :: List1 a -> List1 b -> List1 c -> List1 (a, b, c)
list1zip3 = list1zipWith3 (,,)

list1zipWith3 :: (a -> b -> c -> d) -> List1 a -> List1 b -> List1 c -> List1 d
list1zipWith3 f (a :| as) (b :| bs) (c :| cs) = f a b c :| zipWith3 f as bs cs

list1unzip4 :: List1 (a, b, c, d) -> (List1 a, List1 b, List1 c, List1 d)
list1unzip4 ((a, b, c, d) :| xs) = (a :| as, b :| bs, c :| cs, d :| ds)
  where
    (as, bs, cs, ds) = unzip4 xs

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
