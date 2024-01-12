
module Utils.Set where

import           Data.Foldable       (foldrM)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Agda.Utils.Function (applyWhen)
import           Agda.Utils.Functor  ((<&>))

filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM p = foldrM step Set.empty
  where
  step x acc = p x <&> \b -> applyWhen b (Set.insert x) acc

for :: Ord b => Set a -> (a -> b) -> Set b
for = flip Set.map
