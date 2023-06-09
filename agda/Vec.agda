
module Vec where

data Nat : Set where
    zero : Nat
    suc  : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

data Vec (A : Set) : Nat → Set where
  nil  : Vec A zero
  cons : {n : Nat} → A → Vec A n → Vec A (suc n)

onetwothree : Vec Nat 3
onetwothree = cons 1 (cons 2 (cons 3 nil))

head : {A : Set}{n : Nat} → Vec A (suc n) → A
head (cons x _) = x

map : {A B : Set}{n : Nat} → (A → B) → Vec A n → Vec B n
map f nil = nil
map f (cons x xs) = cons (f x) (map f xs)


