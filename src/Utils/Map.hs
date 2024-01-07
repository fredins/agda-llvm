
module Utils.Map where

import           Data.Map (Map)
import qualified Data.Map as Map

adjustWithDefault :: Ord k => k -> (v -> v) -> v -> Map k v -> Map k v
adjustWithDefault k f v xs | Map.member k xs = Map.adjust f k xs
                           | otherwise = Map.insert k v xs
