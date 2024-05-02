module Formalize.Utils.Erase where

mapSig :: (p -> q) -> p -> q
mapSig g s = g s

