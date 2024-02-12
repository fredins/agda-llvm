---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

  ```agda
  module logbook where

  open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 

  private 
    variable
      A B : Set

  infixr 5 _∷_
  data List A : Set where
    []  : List A
    _∷_ : A → List A → List A

  ```

### W.5 and W.6


Read the following: 

- @cockx2022
- @reinking2020 (proofs)

Did the following: 

- Finished the new code generator, which correctly handles nested case  
  expressions among other things.

- Defined some provable/testable properties about our Perceus algorithm.

### W.3 and W.4


- Reworked how updates are done throughout the compiler:

  1. For the Perceus algorithm to be sound we need to know  
     the current tag when performing an update. Hence,  
     we move all updates into the case expression inlined by  
     eval. Another benlefit of this is that we avoid all  
     unecessary updates (when there is already an C-tag).

  2. If the function can return multiple tags, then we   
     insert another case expression to determine the new  
     tag. 

  Example: 
 
  ```markdown
  sum x1 = 
    fetch 0 [1] ; λ x2 → 
    (case 0 o
      C[]  → unit (C[])
      C_∷_ → 
        fetch 1 [2] ; λ x3 →
        fetch 2 [3] ; λ x4 →
        unit (C_∷_ 1 0)
      FdownFrom → 
        fetch 1 [1] ; λ x5 →
        downFrom 0  ; λ x6 x7 x8 →
        (case 2 of
           C[]  → update 4 (C[]) 
           C_∷_ → update 4 (C_∷_ 1 0) 
        ) ; λ () → 
        unit (2 1 0)
    ) λ x9 x10 x11 → 
    case 2 of
      C[] → 
        ...
      C_∷_ → 
        ...
  ```

  Previously updates looked like this:

  ```markdown
  sum x1 = 
    fetch 0 [1] λ x2 → 
    (case 0 o
      C[]  → unit (C[])
      C_∷_ → 
        fetch 1 [2] λ x3 →
        fetch 2 [3] λ x4 →
        unit (C_∷_ 1 0)
      FdownFrom → 
        fetch 1 [1] λ x5 →
        downFrom 0 λ x6 x7 x8 →
        unit (2 1 0)
    ) λ x9 x10 x11 → 
    case 2 of
      C[] → 
        update 4 (C[]) λ () → 
        ...
      C_∷_ → 
        update 4 (C_∷_ 1 0) λ () → 
        ...
  ```

- Updated the Perceus rules to reflect the new update approach.  
  The new rules are fewer and not as ad hoc.

- Refactored many of the previous GRIN transformations, and added 5 new  
  transformations: _constant propagation_, _fetch_reuse_, _drop raising_,  
  _dup_lowering_, and a new _dup/drop fusion_.

- Started working on an new LLVM IR codegen.

- Experimented with "trees-that-grow".

- How to demonstrate correctness of the Perceus algorihm?

  Option 1: Prove some properties of the algorihm in Agda  
            and use agda2hs to derive the normal haskell  
            function. 

    ```txt
    + Cool  
    + Mechanically and statically verified  
    + Only have to prove the Perceus algorithm  
    + Can postulate necessary invariants  
    - Hard and tedious to prove things  
    - I've little experience writing big proofs
    ```
  
  Option 2: Property-based testing using QuickCheck.  
            Generate the initial GRIN code and then  
            run the points-to analysis and all the  
            transformations.

    ```txt
    + Tests the the whole compiler
    - Tests the the whole compiler
    - GRIN is untyped, so how do we create valid GRIN programs?  
    ```
  
  Option 3: Write manual proofs.  

    ```txt
    - High likelihood of making a mistake
    - Little experience writing complex proofs
    ```


### W.47-50 

Did the following:

 - Finished the course report.

 - Some fixes to the compiler.

### W.46

Read the following: 

- @wadler1987
- @reynolds1972
- @hausmann2015
- @pepels1988cyclic
- @kaser1992
- @mycroft1980
- more...

Did the following: 

- I did a complete refactoring of the interpreter, and added a `highest_allocation` 
  statistic.

- I modified the program from the previous week, which reduces the worst case space  
  complexity from roughly half of the input size to a constant of five nodes regardless  
  of the input size. I have tested this with the interpreter for small instances, and  
  with valgrind using the tool massif on large instances. With an instance of 1 million  
  the old program uses 60MiB at peak memory usage meanwhile the new one only uses 128B  
  (not sure why not 8\*4\*5=160).  

  ```agda
  module DownFromOpt where
    open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
    open import Agda.Builtin.Strict using (primForce)

    {-# TERMINATING #-}
    downFrom : ℕ → List ℕ
    downFrom zero = []
    downFrom (suc n) = primForce n (λ n → n ∷ downFrom n)

    sum : ℕ → List ℕ → ℕ
    sum acc [] = acc
    sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

    main = sum 0 (downFrom 100) 
  ```

  The key was making `downFrom` not tail recursive to make the list lazy generated.  
  Then, I made sure to force the value `n` which otherwise is the thunk `n - 1`  
  (where `n` is the `suc n`). By doing this, we can drop the two references to `n` in   
  `downFrom`. Therefore, the `n` in `sum` will have a reference count of 1 which means   
  it can be freed by `_+_` after evaluating the next accumulator `primForce n _+_ acc`.  

- I think this example nicely shows of the effects of combining precise reference counting   
  with laziness. Due of the pulling nature of lazy evaluating we never have to create   
  the entire list, and precise reference counting frees the pulled object as soon as   
  we are done with it.   

- Removed the agda submodule

- Report stuff.

### W.44 W.45

Read the following: 

- @wadler1988
- @johnson2017

Did the following:

- Started on a new solver for the heap points-to analysis which uses depth-first ordering [@aho2006]  
  which was recommened by [@boquist1996]. The new solver should converge for all programs  
  which is not  the case for the current one. The new solver would also redcuce the number of  
  fix-point interations. The new algorithm is implemented but something is wrong; it doesn't  
  even terminate for our test program. I need to look into this once I have more time.  

- Implemented `primForce`{.agda} which enables strictness. Now, we can avoid stack overflows completely by  
  making the accumlator strict.  

  ```agda
  module DownFromTail′ where

    open import Agda.Builtin.Nat using (suc; zero; _+_) renaming (Nat to ℕ) 
    open import Agda.Builtin.Strict using (primForce)

    downFrom : List ℕ → ℕ → List ℕ
    downFrom acc zero    = acc
    downFrom acc (suc n) = downFrom (n ∷ acc) n

    sum : ℕ → List ℕ → ℕ
    sum acc [] = acc
    sum acc (x ∷ xs) = sum (primForce x _+_ acc) xs

    -- Your computer's memory is the limit!
    main = sum 0 (downFrom [] 10_000_000)
  ```
- I have updated the Github README somewhat.

- I have fixed bunch things in the report based on Patrik's feedback. There is now a section for  
  related work in the report. I have also started to refactor parts of Sections 1 and 2.  


### W.38 & W.39

Read the following:

 - @teeuwissen2023
 - @wadler1984

Did the following: 

- Modified the Perceus algorithm so arguments are dupped by caller instead of the callee (See report of details).

- Implemented _drop specialization_ and _dup/drop fusion_ to avoid reference counting operations (See report for details).

- Wrote sections 2, 3, 4, and 5. The report is basically finished but it lacks a related works section.

### W.37

Read the following:

 - @johnsson1991
 - @davis1991
 - @jonsson2008
 - @bolingbroke2010
 - @petersen2013
 - @liu2013
 - @bergstrom2010
 - @kaser1997
 - @johnsson1984

Did the following:

- Added allocation tag counts to the interpreter.

  ```txt
  Result: 4950
  Allocations Cnat: 101
              FDownFrom.downFrom: 101
              FAgda.Builtin.Nat._-_: 100
              FDownFrom.sum: 100
              Total: 402
  In use at exit: ∅
  ```

- Added tailcalls with the `tail` attribute in LLVM. LLVM has a `musttail` for  
  garanteed tailcalls but this only works for some calls even if they are in  
  tail call positions. I did this to avoid stack overflows. It worked partially  
  now the program stack overflows at input of 74 000 elements instead of 58 000.  
  Another intresting result is that the allocations basically didn't change, however,  
  different tags where allocated.  
  

  ```agda
  module DownFromTail where

    downFrom : List ℕ → ℕ → List ℕ
    downFrom acc zero    = acc
    downFrom acc (suc n) = downFrom (n ∷ acc) n

    sum : ℕ → List ℕ → ℕ
    sum acc [] = acc
    sum acc (x ∷ xs) = sum (x + acc) xs

    -- Current max: 74 000
    main = sum 0 (downFrom [] 100)
  ```

  ```txt 
  Result: 4950
  Allocations Cnat: 102
              CDownFromTail.List._∷_: 100
              FAgda.Builtin.Nat._+_: 100
              FAgda.Builtin.Nat._-_: 100
              CDownFromTail.List.[]: 1
              FDownFromTail.downFrom: 1
              Total: 404
  In use at exit: ∅
  ```

- I finished the abstract and the introduction in the report, and continued writing  
  on the rest of the sections.  

### W.35 & W.36


Read the following:  

  - @lorenzen2021
  - @pinto2023  

Did the following:  

- Created a new transformation called fetch specialization, which is similiar to  
  Boquist's update specialization. The motivation for this transformation is the  
  conflict between Perceus algorithm [@reinking2020] and the Cnat tag.  
  Perceus assumes that all  offsets 2≥ are pointers, and should therefore be reference  
  counted. This is true for all tags except Cnat which stores a integer at offset 2.  

  ```markdown
  Cnat →
    dup 2 ; λ () →
    fetch 2 [2] ; λ x3716 →
    drop 0 ; λ () →  -- BAD: drop takes a pointer but offset 2 of Cnat is an integer
    unit (Cnat 0)

  >>> 

  Cnat →
    dup 2 ; λ () →
    fetchCnat 2 [2] ; λ x3716 →
    unit (Cnat 0)
  ```

  The transformation is easily integrated in the eval inlining and fetch right-hoisting transformations.  
  Another satisfactory property of this change is the symmetry between `fetch` and `update`.  
  Moreover, I think fetch specialization may enable very powerful optimizations. For example,  
  it could enable per-node custom data layouts for the arguments, which would alleviate GRIN's  
  weakness of requiring a uniform node layout. However, a uniform data layout may be more advantageous  
  for memory resue between different nodes.  

- Implemented precise reference counting mostly based on the Perceus algorithm, but I had to change  
  mulitple parts because of the difference between GRIN and the calculus used by @reinking2020.  
  One big difference is that my implementation needs to deal with borrowing for the primitives  
  `store`, `fetch`, and `update`. This should not consume references similiar to how `dup` and `drop`  doesn't.  
  However, borrowing in `dup` and `drop` doesn't interfere with the algorithm which GRIN's primitives  
  do. The only operation which consumes values are function calls and returning `unit`. Another difference,   
  is that in GRIN the return value of a function is not allocated by the callee. Instead it is passed in  
  registers to the caller. Following is the `downFrom` function with reference counting operations.  

  ```markdown
  DownFrom.downFrom x7 =
  fetch 0 [1] ; λ x56 →
  (case 0 of
     Cnat →
       fetchCnat 1 [2] ; λ x72 →
       unit (Cnat 0)
     FAgda.Builtin.Nat._-_ →
       fetchFAgda.Builtin.Nat._-_ 1 [2] ; λ x73 →
       fetchFAgda.Builtin.Nat._-_ 2 [3] ; λ x74 →
       Agda.Builtin.Nat._-_ 1 0
  ) ; λ Cnat x55 →
  updateCnat 2 (Cnat 0) ; λ () →
  case 0 of
    0 →
      drop 2 ; λ () →
      unit (CDownFrom.List.[])
    _ →
      store (Cnat #1) ; λ x2 →
      store (FAgda.Builtin.Nat._-_ 3 0) ; λ x5 →
      dup 0 ; λ () →
      store (FDownFrom.downFrom 0) ; λ x4 →
      dup 0 ; λ () →
      dup 1 ; λ () →
      unit (CDownFrom.List._∷_ 1 0)
  ```

- I have fixed both the interpreter and the code generator so they work with reference counting.  
  The interpreter now reports the status of the heap after execution. We can also check the memory  
  usage of the compiled binary with the program `valgrind`, which output is shown below. The  
  result shows that the reference counting operations successfully deallocated all allocated memory.  
  Another intresting information is that there were 403 allocation for the program. The same Haskell  
  program only require 49 allocations. This is definitely due to the demand analyzer of Haskell which  
  makes some operations strict which avoid allocations for thunks . Our implementation doesn't yet  
  have  such an analysis. Another way to minimize the amount of allocations is to implement drop  
  specialization and reuse analysis in Perceus.  
  

  ```markdown
  ==10406== memcheck, a memory error detector
  ==10406== copyright (c) 2002-2022, and gnu gpl'd, by julian seward et al.
  ==10406== Using Valgrind-3.20.0 and LibVEX; rerun with -h for copyright info
  ==10406== Command: ./program
  ==10406==
  4950
  ==10406==
  ==10406== HEAP SUMMARY:
  ==10406==     in use at exit: 0 bytes in 0 blocks
  ==10406==   total heap usage: 403 allocs, 403 frees, 13,888 bytes allocated
  ==10406==
  ==10406== All heap blocks were freed -- no leaks are possible
  ==10406==
  ==10406== For lists of detected and suppressed errors, rerun with: -s
  ==10406== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
  ```


### W.34

Read the following:  
  
- @boquist1995
- @collins1960
- @appel1997
- @jones1996
- @peytonjones1992
- @wilson1992

Did the following:  

- Primitive operations are now wrapped in a function which forces the operands: 

  ```markdown 
  Agda.Builtin.Nat._+_ r30 x28 x29 =
   eval 1 ; λ Cnat x27 →
   eval 1 ; λ Cnat x26 →
   PAdd 1 0 ; λ x25 →
   unit (Cnat 0)
  ```

- Removed the register introduction transformation (see [W.33]) because LLVM IR does  
  not allow literals to be assigned to registers. For example, `%1 = 100`{.llvm} is not allowed.  
  Addionally, It doesn't make the LLVM code generator easier to implement, unlike  
  the RISC code generator by @boquist1999. The register introduction is, however,  
  a precondition for the (unimplemented) common sub-expression elimination optimization.  

- Finished implementing the code generator.  

- Started refactoring the treeless-to-GRIN phase using de Bruijn substitution instances,  
  and ReaderT instead of StateT. However, the refactor is currently archived and put on  
  hold. 

- I wrote another Agda test program which is more suitable for testing reuse. This program  
  required "Returning eval" in the first clause of `mapDouble`{.agda} [@boquist1999, p. 95].

  ```agda
  module Example where

    downFrom : ℕ → List ℕ
    downFrom zero    = []
    downFrom (suc n) = n ∷ downFrom n 

    mapDouble : List ℕ → List ℕ
    mapDouble []       = []
    mapDouble (x ∷ xs) = x + x ∷ mapDouble xs

    sum : List ℕ → ℕ
    sum []       = 0
    sum (x ∷ xs) = x + sum xs

    main = sum (mapDouble (downFrom 10000)) 
  ```

- Rewrote the introduction in the report. 


### W.33

Read the following:  
  
- @racordon2021
- @racordon2022

Did the following:  

- Implemented the right hoisting fetch tranformation, which hoists fetches  
  with offset > 0 into the the appropriate case alternatives. 

  ```markdown
  fetch 0 [0] ; λ x10 →
  fetch 1 [1] ; λ x9 →
  fetch 2 [2] ; λ x8 →
  (case 2 of
     FPrim.Sub →
       PSub 1 0 ; λ x0 →
       unit (Cnat 0)
     Cnat → unit (Cnat 1)
  ) ; λ Cnat x7 →
  updateCnat 4 (Cnat 0) ; λ () →
  <m>

  -- >>>
  
  fetch 0 [0] ; λ x10 →
  (case 0 of
     FPrim.Sub →
       fetch 1 [1] ; λ x11 →
       fetch 2 [2] ; λ x13 →
       PSub 1 0 ; λ x0 →
       unit (Cnat 0)
     Cnat →
       fetch 1 [1] ; λ x12 →
       unit (Cnat 0)
  ) ; λ Cnat x7 →
  updateCnat 2 (Cnat 0) ; λ () →
  <m>
  ```

- Implemented the last simplifying (and necessary) tranformation: register introduction.  
  The tranformation gives names to all values, and ensures that arguments to (e.g.) the  
  `store` operation only contains variables.  
  
  ```markdown
  storel21 (Cnat #100) ; λ x24 →
  storel22 (FDownFrom.downFrom 0) ; λ x23 →
  DownFrom.sum 0 ; λ Cnat x25 →
  <m>
  
  -- >>>
    
  unit #100 ; λ x3690 →
  unit (Cnat 0) ; λ x3689 →
  storel21 0 ; λ x24 →
  unit (FDownFrom.downFrom 0) ; λ x3691 →
  storel22 0 ; λ x23 →
  <m>
  
  ```

- Implemented approx. 90% of the code generator.

### W.32

Read the following:  

  - @xi2018
  - more...

Did the following:

- Added build instructions and dependencies to README.

- Major refactoring. Datatype Term was split into Term and Val. Alt was split 
  into LAlt (lambda alternative) and CAlt (case alternative).

- Fixed various issues in (e.g.) the sharing analysis and the substitution instances.

- Implemented vectorization which replaces node variables with _explicit_ nodes. This 
  is an important transformation for the code generation.  

  ```markdown
  <t1> ; λ x₁ →
  <t2>

  -- >>>

   <t1> ; λ tag x₂ x₃ →
   <t2> [tag x₂ x₃ / x₁]
  ```

- Implemented case simplification. After the transformation, case expression only 
  scrutinize tag variables, and case patterns do not bind any variables.  

  ```markdown
   <t1>
   case tag x₁ x₂ of
     Cnil        → <t2>
     Ccons x₃ x₄ → <t3>
   
   -- >>>
   
   <t1>
   case tag of
     Cnil  → <t2>
     Ccons → <t3> [x₁ / x₃, x₂ / x₄]

  ````

- Implemented fetch splitting using offsets:  

  ```markdown
   <t1>
   fetch p ; λ tag x₁ x₂ →
   <t2>
   
   -- >>>
   
   <t1>
   fetch p [0]; λ tag →
   fetch p [1]; λ x₁ →
   fetch p [2]; λ x₂ →
  ```

- Did a lot of work on the right-hoisting fetch operations, however, I encountered some issues 
  with substitutions and de Bruijn indices.  

- Properly configured the `lagda.tex` document for the report. 

- Started writing the introduction and noted down ideas for the related work section.


### W.30 & W.31

Did the following:

- Implemented instances for `Subst` and `DeBruijn` for GRIN, which  
  makes it easier to correctly manipulate DeBruijn indices.  

- Implemented normalisation for GRIN expression, to make the monadic  
  binds right-skewed.  

  ```markdown
  store (Cnil) λ x0 →
  ( store (Cnat #1) ; λ x1 →
    store (FdownFrom 0) ; λ x2 →
    unit 0
  ) ; λ x3 →
  unit (Ccons 1 0)

  -- >>>

  store (Cnil) λ x0 →
  store (Cnat #1) ; λ x1 →
  store (FdownFrom 0) ; λ x2 →
  unit 0 ; λ x3 →
  unit (Ccons 3 0)
  ```

- Set up testing, and tested the normalisation using 'golden testing'.  

- Implemented update specialization transformation, which converts  
  general (unknown tag) `update` to a tag-specialized operation `updateᶜᶜᵒⁿˢ`.  
  However, there is currently a small bug in the implementation.

  ```markdown
  update v₁ v₂ ; λ () →
  〈m₁ 〉
  case v₂ of
    CNil       → 〈m₂ 〉
    CCons x xs → 〈m₃ 〉

  -- >>>

  〈m₁ 〉
  case v₂ of
    CNil       →
      updateᶜᴺⁱˡ v₁ v₂ ; λ () →
      〈m₂ 〉
    CCons x xs →
      updateᶜᶜᵒⁿˢ v₁ v₂ ; λ () →
      〈m₃ 〉
  ```

- Implemented a GRIN interpreter to check that the program produces the expected result,  
  and to check that the transformations does not change the program semantics. In doing  
  this, I discovered and fixed multiple bugs. I also gained a better understanding on how  
  allocations work in GRIN.  

### W. 29

Read the following papers:

- @hage2008
- @ende2010
- @dijkstra2009
- @boeijink2010
- @johnsson2004
- @podlovics2021
- @huang2023
- @okabe2014

Did the following:

- Rewrote the heap points-to analysis and incorporated the sharing analysis.  

  ```markdown
  ------------------------------------------------------------------------
  -- * GRIN
  ------------------------------------------------------------------------

  DownFrom.downFrom r9 x8 =
    eval 0 ; λ Cnat x7 →
    case 0 of
      0 → unit (CDownFrom.List.[])
      _ → storel0 (Cnat #1) ; λ x3 →
          storel1 (FPrim.sub 2 0) ; λ x2 →
          storel4 (FDownFrom.downFrom 0) ; λ x5 →
          unit (CDownFrom.List._∷_ 4 0)

  DownFrom.sum r20 x19 =
    eval 0 ; λ x18 →
    case 0 of
      CDownFrom.List.[] → unit (Cnat #0)
      CDownFrom.List._∷_ x16 x17 →
        storel10 (FDownFrom.sum 0) ; λ x11 →
        eval 0 ; λ x15 →
        eval 3 ; λ x14 →
        PAdd 1 0 ; λ x13 →
        unit 0

  DownFrom.main =
    storel21 (Cnat #4) ; λ x24 →
    storel22 (FDownFrom.downFrom 0) ; λ x23 →
    DownFrom.sum 0 ; λ Cnat x25 →
    printf 0

  ------------------------------------------------------------------------
  -- * Heap points-to analysis
  ------------------------------------------------------------------------

  Abstract heap:
  l0 → Cnat [BAS]
  l1 → FPrim.sub [l1 ∪ l21, l0]
  l4 → FDownFrom.downFrom [l1]
  l10 → FDownFrom.sum [l4]
  l21 → Cnat [BAS]
  l22 → FDownFrom.downFrom [l21]

  Abstract env:
  x2 → l1
  x3 → l0
  x5 → l4
  x7 → BAS
  x8 → l1 ∪ l21
  x9 → CDownFrom.List._∷_ [l1 ∪ l21, l4] ∪ CDownFrom.List.[] []
  x11 → l10
  x13 → Cnat [BAS]
  x14 → Cnat [BAS]
  x15 → Cnat [BAS]
  x16 → l1 ∪ l21
  x17 → l4
  x18 → CDownFrom.List._∷_ [l1 ∪ l21, l4] ∪ CDownFrom.List.[] []
  x19 → l4 ∪ l22
  x20 → Cnat [BAS]
  x23 → l22
  x24 → l21
  x25 → BAS

  Shared: {0, 1, 2, 3, 8, 21, 24}
  ```
- Implemented eval inlining which uses the heap points-to analysis to 
  generate specialized eval functions for each call.  

  ```markdown

  ------------------------------------------------------------------------
  -- * Inlining eval
  ------------------------------------------------------------------------

  DownFrom.downFrom r9 x8 =
    (fetch 0 ; λ x30 →
     (case 0 of
        CDownFrom.List._∷_ x27 x28 → unit 0
        CDownFrom.List.[] → unit 0
     ) ; λ x29 →
     update 2 0 ; λ () →
     unit 0
    ) ; λ Cnat x7 →
    case 0 of
      0 → unit (CDownFrom.List.[])
      _ → storel0 (Cnat #1) ; λ x3 →
          storel1 (FPrim.sub 2 0) ; λ x2 →
          storel4 (FDownFrom.downFrom 0) ; λ x5 →
          unit (CDownFrom.List._∷_ 4 0)

  DownFrom.sum r20 x19 =
    (fetch 0 ; λ x34 →
     (case 0 of
        CDownFrom.List._∷_ x31 x32 → unit 0
        CDownFrom.List.[] → unit 0
     ) ; λ x33 →
     update 2 0 ; λ () →
     unit 0
    ) ; λ x18 →
    case 0 of
      CDownFrom.List.[] → unit (Cnat #0)
      CDownFrom.List._∷_ x16 x17 →
        storel10 (FDownFrom.sum 0) ; λ x11 →
        (fetch 0 ; λ x37 →
         (case 0 of
            FDownFrom.sum x35 → DownFrom.sum 0
         ) ; λ x36 →
         update 2 0 ; λ () →
         unit 0
        ) ; λ x15 →
        (fetch 3 ; λ x42 →
         (case 0 of
            FPrim.sub x38 x39 → Prim.sub 1 0
            Cnat x40 → unit 0
         ) ; λ x41 →
         update 2 0 ; λ () →
         unit 0
        ) ; λ x14 →
        PAdd 1 0 ; λ x13 →
        unit 0

  DownFrom.main =
    storel21 (Cnat #4) ; λ x24 →
    storel22 (FDownFrom.downFrom 0) ; λ x23 →
    DownFrom.sum 0 ; λ Cnat x25 →
    printf 0
  ```
- Read papers that have cited Boquist's writings about GRIN (see [Related work]). 

- I have decided to shift the focus from LLVM to GRIN because all of the planned  
  optimization uses GRIN. Addionally, the GRIN-to-LLVM transformation is uncomplicated,  
  and can be easily substituted for a C backend or a native code backend.
  
  
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
  module DownFrom where

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

### Related work

- @ullrich2021 proposes a mechanism for reclaiming non-shared allocations, and an  
  approach to minimize reference counts operations through borrowed refereces. Owned  
  references need to be consumed exactly once. A reference is consumed when it is  
  returned, stored as a heap value, and passed to another function (as an owned reference).  
  Addionally, there is `dec` and `inc` for consuming and copying references,   
  respectively. A borrowed references is not allowed to be consumed.   


- @hage2008 presented a destructive assignment operator `variable@constructor-expr`  
  to indicate when to reuse. The operator is designed to be compatible with existing  
  languages' memory models and memory strategies. Safety is garanteed through a uniqueness  
  analysis. One limiting aspect is that reuse is only prohibited if the variable is unique  
  and the constructors are identical. So, for example, an cons-node can only be reused by  
  another cons-node.

  ```haskell
  rev []         acc = acc
  rev l@(x : xs) acc = rev xs l@(x : acc)
  ```

  The authors mention GRIN's approach which enables reuse between nodes of different tags.  
  This is accomplished by a uniform node representation called the _small layout_ which can be  
  for example 3 words. Nodes with more than 3 - 1 arguments are split into two parts. The first  
  part can reuse an allocation whereas the second part needs a fresh allocation. Another aspect of  
  the GRIN mermory model is that there cannot be a node which is smaller than the small layout,  
  which is unfortunate because, for instance, the nil-node only require 1 word but occupy 3.

  In comparison to @hage2008, my approach aims for automatic reuse without any  
  syntax annotations. Thereby, optimization are applied to all programs, and the programmer do  
  not have to think about memory implementation details. On the other hand, this project does  
  not attempt to be compatible with current programming languages' compilers and runtime systems.  

  Another difference is that the system presented by @hage2008 require modifications  
  to the type system for the uniqueness (or sharing) analysis. In GRIN the analysis is trivial, and does  
  not require changes to the type system because all variables are available due to whole-program compilation.
  
- The Utrecht Haskell Compiler used GRIN and LLVM [@dijkstra2009; @ende2010]. However, they  
  seem to have extended GRIN with A tags for applications of higher order functions, and H tags for holes.  

- @johnsson2004 suggest that there are more efficient implementation of lazy functional languages than  
  the G-machine, or any of the variations (including GHC's STG machine). Addionally, he suggests GRIN as  
  a more efficent alternative.

- @huang2023 present the first formally-specified type-preserving defunctionalization translation  
  for dependenlty-typed languages. Defunctionalization transforms higher-order program into first-order  
  program, and it is used extensively in GRIN. Ideas from this paper may be useful for developing a type  
  system for GRIN that will work with dependenlty-typed languages, such as Agda.

- The Ajhc uses GRIN to compile Haskell to C. It has been used to program a NetBSD sound driver in  
  Haskell [@okabe2014].

References
----------

