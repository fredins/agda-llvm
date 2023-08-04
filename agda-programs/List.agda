
module List where

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A


data Bool : Set where
  false : Bool
  true  : Bool

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

any : {A : Set} → (A → Bool) → List A → Maybe A
any p [] = nothing
any p (x :: xs) with p x 
...                | true  = just x
...                | false = any p xs


