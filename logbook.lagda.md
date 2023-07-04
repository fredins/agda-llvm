---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---


### W. 27 and prior

Read the following papers:  
- @jones1992 

Did the following:

- Tried to implement STG-like thunks manually in LLVM IR.  
- Manually translated the the following program first GRIN and then
  LLVM IR.  

  ```agda  

  open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

  infixr 5 _∷_
  data List {a} (A : Set a) : Set a where
    []  : List A
    _∷_ : (x : A) (xs : List A) → List A

  downFrom : ℕ → List ℕ
  downFrom zero = []
  downFrom (suc n) = suc n ∷ downFrom n 
  
  sum : List ℕ → ℕ
  sum [] = 0
  sum (x ∷ xs) = x + sum xs
  
  main = sum (downFrom 4)
  ```

- Checked out literate agda using both markdown and latex.  
- Prepared the documents for the logbook and the report.  

References
----------

