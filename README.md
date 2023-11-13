An Agda backend which compiles to LLVM via the intermidate representation GRIN [Boquist 1999, Johnsson 1991].  

### Goal of the project

The goal is to compile Agda to GRIN, and to extend GRIN with `dup` and `drop` from Perceus's [Reinking et al. 2021] linear calculus. This will allow precise reference counting and memory reuse.  

### Example programs

```agda
module DownFrom where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : ℕ → List ℕ
downFrom zero = []
downFrom (suc n) = n ∷ downFrom n

sum : List ℕ → ℕ
sum [] = 0
sum (x ∷ xs) = x + sum xs

-- Stack overflow when > 58 000
main = sum (downFrom 10_000) 
```
Tail recursive and non-leaky variant:

```agda
module DownFromTail where

open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict using (primForce)

infixr 5 _∷_
data List A : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

downFrom : List ℕ → ℕ → List ℕ
downFrom acc zero    = acc
downFrom acc (suc n) = downFrom (n ∷ acc) n

sum : ℕ → List ℕ → ℕ
sum acc [] = acc
sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

-- Your computer's memory is the limit!
main = sum 0 (downFrom [] 10_000_000)
```

### Restrictions
- No lambdas.  
- No higher-order functions or partial applications.  
- No parametric polymorphism.  
- Only pure functions.

### Known issues
- Super ugly code.  
- Heap points-to analysis doesn't terminate for all programs.  
- All functions in scope must be reachable from `main`. This 
  includes imports, and `using`/`hiding` have no effect.  
- Only a small number of primitives are supported (`NATURAL`, `NATPLUS`, `NATMINUS`).
- Integer overflows.
- There is no strictness/demand analysis and all functions calls are lazy. Therefore,  
  space leaks are common and evaluating a huge thunk blows up the stack. This can be 
  manually mitigated by using `primForce`.
- Proofs are not erased properly.

### Dependencies

- ghc
- cabal-install
- zlib

### Build from source

The project depends on features from the unreleased Agda 2.6.4, which is why there is an agda submodule. If you already
have a local installation of Agda you may instead modify the `cabal.project` file, and skip the submodule.  

```
git clone --recurse-submodules git@github.com:fredins/agda-llvm.git
```

Running `make` will build and install the exectuable `agda-llvm` usually to `$HOME/.cabal/bin/`. This will build all dependencies and the agda submodule, which is over 400 modules. 

To run the compiler use the `--llvm` flag.  

```
agda-llvm --llvm agda-programs/DownFrom.agda
```

Many programs use the standard library which needs to be installed and configured separately, see [agda.readthedocs.io/en/latest/getting-started/installation.html](https://agda.readthedocs.io/en/latest/getting-started/installation.html) and [agda.readthedocs.io/en/latest/tools/package-system.html#use-std-lib](https://agda.readthedocs.io/en/latest/tools/package-system.html#use-std-lib).  



### Logbook
See [fredins.github.io](https://fredins.github.io)

### Paper (WIP)

See `latex/report.pdf`

### References
See `bibliography.bib`
