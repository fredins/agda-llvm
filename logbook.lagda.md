---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

### W. 28

Read the following papers:

- @boquist1996
- @aho2006

Did the following:

- Implemented the heap points-to analysis, which gives a safe approximation
  of the possible values of each abstract heap location. 
  It also determines the possible values of each variable.  

  ```markdown

  variable                                            location
  ------------------------------------------------------------
  x1      DownFrom.downFrom #1 =
  x2        eval @0 ; λ Cnat #1 →
            case @0 of
              0 → unit (CDownFrom.List.[])
  x3          _ → store (Cnat 1) ; λ #1 →                   l0
  x4              store (FPrim.sub @2 @0) ; λ #1 →          l1
  x5              store (FDownFrom.downFrom @0) ; λ #1 →    l2
                  unit (CDownFrom.List._∷_ @4 @0) 

  ... rest of the program ...
  

  Heap points-to analysis
  ------------------------------------------------------------
  Solved heap:
  l0 → Cnat [BAS]
  l1 → FPrim.sub [l1 ∪ l4, l0]
  l2 → FDownFrom.downFrom [l1] ∪ CDownFrom.List.[] [] ∪ CDownFrom.List._∷_ [l1 ∪ l4, l2]

  Solved env:
  x0 → CDownFrom.List.[] [] ∪ CDownFrom.List._∷_ [l1 ∪ l4, l2]
  x1 → l1 ∪ l4
  x2 → BAS
  x3 → l0
  x4 → l1
  x5 → l2
  ```
- Started incorporating sharing analysis which makes the heap points-to 
  analysis more precise.

### W. 27 and prior

Read the following papers:  

- @jones1992 
- @boquist1999
- @reinking2020
- more... 

Did the following:

- Tried to implement STG-like thunks manually in LLVM IR.  
- Manually translated the the following program first to GRIN and then
  to LLVM IR.  

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

- Created a transformation for the treeless syntax that 
  simplify applications.

  ```markdown
  f a (g b) (h c)
  -- >>>
  let b' = g b in
  let c' = h c in
  f a b' c'
  ```
- Implemented treeless-to-GRIN transformation for the example program, 
  and started on the GRIN-to-LLVM transformation.
- Checked out literate agda using both markdown and latex.  
- Prepared the documents for the logbook and the report.  

References
----------

