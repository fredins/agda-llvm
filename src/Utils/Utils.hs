module Utils.Utils
  ( (??)
  , (.:)
  , (.:.)
  , caseEither
  , swap01'
  , printPretty
  , trace'
  , logIO
  , forAccumR
  , mapAccumM
  , forAccumM
  , prettySrcLocShort
  , prettyCallSiteShort
  , prettyCallStackShort
  ) where

import           Control.Monad                (liftM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Coerce                  (Coercible, coerce)
import           Data.Functor                 (($>))
import           Data.List                    (intercalate, mapAccumR)
import           System.IO.Unsafe             (unsafePerformIO)

import           Agda.Syntax.Common.Pretty
import           Agda.TypeChecking.Substitute
import           Agda.Utils.CallStack

-- TODO migrate rest of the functions to new modules

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

infixr 8 .:

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(f .:. g) x y z = f (g x y z)

infixr 8 .:.

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
trace' s = unsafePerformIO . (logIO s $>)

logIO :: MonadIO m => String -> m ()
logIO s = liftIO $ appendFile "trace.log" (s ++ "\n")

forAccumR :: Traversable t => s -> t a -> (s -> a -> (s, b)) -> (s, t b)
forAccumR s t f = mapAccumR f s t

prettySrcLocShort :: SrcLoc -> String
prettySrcLocShort SrcLoc{srcLocFile, srcLocStartLine, srcLocStartCol} =
  srcLocFile ++ ":" ++ show srcLocStartLine ++ ":" ++ show srcLocStartCol

prettyCallSiteShort :: CallSite -> String
prettyCallSiteShort (fun, loc) = fun ++ ", called at " ++ prettySrcLocShort loc

-- | Pretty-print a @CallStack@. This has a few differences from @GHC.Stack.prettyCallStackLines@.
-- We omit the "CallStack (from GetCallStack)" header line for brevity.
-- If there is only one entry (which is common, due to the manual nature of the @HasCallStack@ constraint),
-- shows the entry on one line. If there are multiple, then the following lines are indented.
prettyCallStackShort :: CallStack -> String
prettyCallStackShort cs = case map prettyCallSiteShort (getCallStack cs) of
  []                  -> "(empty CallStack)"
  firstLoc : restLocs -> intercalate "\n" (firstLoc : map ("  " ++) restLocs)

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
