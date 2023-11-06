{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeInType #-}

module Agda.Llvm.GrinTypes where

type List a = [a]
type Nat = Int


data Memory = MkMemory
data Io = MkIo
data Pure = MkPure


type Type = Type' Pure

data Type' a where
  Nat     :: Type 
  Bool    :: Type
  Pointer :: Type 
  Unit    :: Type 
  Named   :: String → Type 
  Array   :: Nat → Nat → Type 
  Struct  :: List Type → Type 
  Memory  :: Type → Type' Memory
  Io      :: Type → Type' Io

