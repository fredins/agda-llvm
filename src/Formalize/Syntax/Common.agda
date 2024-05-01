
module Formalize.Syntax.Common where

data Tag : Set where
  CNat : Tag

{-# COMPILE AGDA2HS Tag deriving Show #-}
