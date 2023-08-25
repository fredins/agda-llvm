---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

### W.34

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

  <pre class="Agda">
  <a id="10586" class="Keyword">open</a> <a id="10591" class="Keyword">import</a> <a id="10598" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="10615" class="Keyword">using</a> <a id="10621" class="Symbol">(</a><a id="10622" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="10625" class="Symbol">;</a> <a id="10627" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="10631" class="Symbol">;</a> <a id="10633" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="10636" class="Symbol">)</a> <a id="10638" class="Keyword">renaming</a> <a id="10647" class="Symbol">(</a><a id="10648" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="10652" class="Symbol">to</a> <a id="10655" class="Datatype">ℕ</a><a id="10656" class="Symbol">)</a> 

  <a id="10662" class="Keyword">infixr</a> <a id="10669" class="Number">5</a> <a id="10671" href="logbook.html#10738" class="InductiveConstructor Operator">_∷_</a>
  <a id="10677" class="Keyword">data</a> <a id="List"></a><a id="10682" href="logbook.html#10682" class="Datatype">List</a> <a id="10687" class="Symbol">{</a><a id="10688" href="logbook.html#10688" class="Bound">a</a><a id="10689" class="Symbol">}</a> <a id="10691" class="Symbol">(</a><a id="10692" href="logbook.html#10692" class="Bound">A</a> <a id="10694" class="Symbol">:</a> <a id="10696" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="10700" href="logbook.html#10688" class="Bound">a</a><a id="10701" class="Symbol">)</a> <a id="10703" class="Symbol">:</a> <a id="10705" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="10709" href="logbook.html#10688" class="Bound">a</a> <a id="10711" class="Keyword">where</a>
    <a id="List.[]"></a><a id="10721" href="logbook.html#10721" class="InductiveConstructor">[]</a>  <a id="10725" class="Symbol">:</a> <a id="10727" href="logbook.html#10682" class="Datatype">List</a> <a id="10732" href="logbook.html#10692" class="Bound">A</a>
    <a id="List._∷_"></a><a id="10738" href="logbook.html#10738" class="InductiveConstructor Operator">_∷_</a> <a id="10742" class="Symbol">:</a> <a id="10744" class="Symbol">(</a><a id="10745" href="logbook.html#10745" class="Bound">x</a> <a id="10747" class="Symbol">:</a> <a id="10749" href="logbook.html#10692" class="Bound">A</a><a id="10750" class="Symbol">)</a> <a id="10752" class="Symbol">(</a><a id="10753" href="logbook.html#10753" class="Bound">xs</a> <a id="10756" class="Symbol">:</a> <a id="10758" href="logbook.html#10682" class="Datatype">List</a> <a id="10763" href="logbook.html#10692" class="Bound">A</a><a id="10764" class="Symbol">)</a> <a id="10766" class="Symbol">→</a> <a id="10768" href="logbook.html#10682" class="Datatype">List</a> <a id="10773" href="logbook.html#10692" class="Bound">A</a>

  <a id="downFrom"></a><a id="10778" href="logbook.html#10778" class="Function">downFrom</a> <a id="10787" class="Symbol">:</a> <a id="10789" href="logbook.html#10655" class="Datatype">ℕ</a> <a id="10791" class="Symbol">→</a> <a id="10793" href="logbook.html#10682" class="Datatype">List</a> <a id="10798" href="logbook.html#10655" class="Datatype">ℕ</a>
  <a id="10802" href="logbook.html#10778" class="Function">downFrom</a> <a id="10811" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="10816" class="Symbol">=</a> <a id="10818" href="logbook.html#10721" class="InductiveConstructor">[]</a>
  <a id="10823" href="logbook.html#10778" class="Function">downFrom</a> <a id="10832" class="Symbol">(</a><a id="10833" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="10837" href="logbook.html#10837" class="Bound">n</a><a id="10838" class="Symbol">)</a> <a id="10840" class="Symbol">=</a> <a id="10842" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="10846" href="logbook.html#10837" class="Bound">n</a> <a id="10848" href="logbook.html#10738" class="InductiveConstructor Operator">∷</a> <a id="10850" href="logbook.html#10778" class="Function">downFrom</a> <a id="10859" href="logbook.html#10837" class="Bound">n</a> 
  
  <a id="sum"></a><a id="10867" href="logbook.html#10867" class="Function">sum</a> <a id="10871" class="Symbol">:</a> <a id="10873" href="logbook.html#10682" class="Datatype">List</a> <a id="10878" href="logbook.html#10655" class="Datatype">ℕ</a> <a id="10880" class="Symbol">→</a> <a id="10882" href="logbook.html#10655" class="Datatype">ℕ</a>
  <a id="10886" href="logbook.html#10867" class="Function">sum</a> <a id="10890" href="logbook.html#10721" class="InductiveConstructor">[]</a> <a id="10893" class="Symbol">=</a> <a id="10895" class="Number">0</a>
  <a id="10899" href="logbook.html#10867" class="Function">sum</a> <a id="10903" class="Symbol">(</a><a id="10904" href="logbook.html#10904" class="Bound">x</a> <a id="10906" href="logbook.html#10738" class="InductiveConstructor Operator">∷</a> <a id="10908" href="logbook.html#10908" class="Bound">xs</a><a id="10910" class="Symbol">)</a> <a id="10912" class="Symbol">=</a> <a id="10914" href="logbook.html#10904" class="Bound">x</a> <a id="10916" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="10918" href="logbook.html#10867" class="Function">sum</a> <a id="10922" href="logbook.html#10908" class="Bound">xs</a>
  
  <a id="main"></a><a id="10930" href="logbook.html#10930" class="Function">main</a> <a id="10935" class="Symbol">=</a> <a id="10937" href="logbook.html#10867" class="Function">sum</a> <a id="10941" class="Symbol">(</a><a id="10942" href="logbook.html#10778" class="Function">downFrom</a> <a id="10951" class="Number">4</a><a id="10952" class="Symbol">)</a>
</pre>
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

