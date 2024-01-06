
module Utils.List1 where

import qualified Data.List        as List
import           Prelude          hiding (scanr, zipWith3)

import           Agda.Utils.List1 (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1 as List1

-- | Breaks up a string into substrings. Returns every maximal
-- subsequence of zero or more characters distinct from @'.'@.
--
-- > splitOnDots ""         == [""]
-- > splitOnDots "foo.bar"  == ["foo", "bar"]
-- > splitOnDots ".foo.bar" == ["", "foo", "bar"]
-- > splitOnDots "foo.bar." == ["foo", "bar", ""]
-- > splitOnDots "foo..bar" == ["foo", "", "bar"]
splitOnDots :: String -> List1 String
splitOnDots "" = "" :| []
splitOnDots ('.' : s) = [] <| splitOnDots s
splitOnDots (c : s) = (c : p) :| ps
  where
  (p :| ps) = splitOnDots s

-- | @scanr@ variant similiar to @foldr@
scanr' :: (a -> b -> b) -> (a -> b) -> List1 a -> List1 b
scanr' _ g (x :| [])      =  g x :| []
scanr' f g (x1 :| x2 : xs) = f x1 (List1.head ys) <| ys
  where
  ys = scanr' f g (x2 :| xs)

zipWith3 :: (a -> b -> c -> d) -> List1 a -> List1 b -> List1 c -> List1 d
zipWith3 f (a :| as) (b :| bs) (c :| cs) = f a b c :| List.zipWith3 f as bs cs

zip3 :: List1 a -> List1 b -> List1 c -> List1 (a, b, c)
zip3 = zipWith3 (,,)

unzip4 :: List1 (a, b, c, d) -> (List1 a, List1 b, List1 c, List1 d)
unzip4 ((a, b, c, d) :| xs) = (a :| as, b :| bs, c :| cs, d :| ds)
  where
    (as, bs, cs, ds) = List.unzip4 xs

lookup :: Eq k => k -> List1 (k, v) -> Maybe v
lookup k = List.lookup k . List1.toList
