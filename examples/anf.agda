
module anf where

data ℕ : Set where
    zero : ℕ
    suc  : ℕ -> ℕ

-- {-# BUILTIN NATURAL ℕ #-}

applyId : {A : Set} → ({B : Set} → B → B) → A → A
applyId f a = f a

higher = applyId (\{A : Set} (x : A) -> x) (suc zero)

extLam : {A B : Set} → A → B → A
extLam = \a -> \b -> a

four = suc (suc (suc (zero)))

const : {A B : Set} → A → (@0 b : B) → A
const a b = a
