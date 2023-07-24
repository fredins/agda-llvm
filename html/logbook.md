---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

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
- Implemented eval inlining which uses the heap points-to analysis points-to  
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
  <a id="5609" class="Keyword">open</a> <a id="5614" class="Keyword">import</a> <a id="5621" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="5638" class="Keyword">using</a> <a id="5644" class="Symbol">(</a><a id="5645" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="5648" class="Symbol">;</a> <a id="5650" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="5654" class="Symbol">;</a> <a id="5656" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="5659" class="Symbol">)</a> <a id="5661" class="Keyword">renaming</a> <a id="5670" class="Symbol">(</a><a id="5671" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="5675" class="Symbol">to</a> <a id="5678" class="Datatype">ℕ</a><a id="5679" class="Symbol">)</a> 

  <a id="5685" class="Keyword">infixr</a> <a id="5692" class="Number">5</a> <a id="5694" href="logbook.html#5761" class="InductiveConstructor Operator">_∷_</a>
  <a id="5700" class="Keyword">data</a> <a id="List"></a><a id="5705" href="logbook.html#5705" class="Datatype">List</a> <a id="5710" class="Symbol">{</a><a id="5711" href="logbook.html#5711" class="Bound">a</a><a id="5712" class="Symbol">}</a> <a id="5714" class="Symbol">(</a><a id="5715" href="logbook.html#5715" class="Bound">A</a> <a id="5717" class="Symbol">:</a> <a id="5719" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="5723" href="logbook.html#5711" class="Bound">a</a><a id="5724" class="Symbol">)</a> <a id="5726" class="Symbol">:</a> <a id="5728" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="5732" href="logbook.html#5711" class="Bound">a</a> <a id="5734" class="Keyword">where</a>
    <a id="List.[]"></a><a id="5744" href="logbook.html#5744" class="InductiveConstructor">[]</a>  <a id="5748" class="Symbol">:</a> <a id="5750" href="logbook.html#5705" class="Datatype">List</a> <a id="5755" href="logbook.html#5715" class="Bound">A</a>
    <a id="List._∷_"></a><a id="5761" href="logbook.html#5761" class="InductiveConstructor Operator">_∷_</a> <a id="5765" class="Symbol">:</a> <a id="5767" class="Symbol">(</a><a id="5768" href="logbook.html#5768" class="Bound">x</a> <a id="5770" class="Symbol">:</a> <a id="5772" href="logbook.html#5715" class="Bound">A</a><a id="5773" class="Symbol">)</a> <a id="5775" class="Symbol">(</a><a id="5776" href="logbook.html#5776" class="Bound">xs</a> <a id="5779" class="Symbol">:</a> <a id="5781" href="logbook.html#5705" class="Datatype">List</a> <a id="5786" href="logbook.html#5715" class="Bound">A</a><a id="5787" class="Symbol">)</a> <a id="5789" class="Symbol">→</a> <a id="5791" href="logbook.html#5705" class="Datatype">List</a> <a id="5796" href="logbook.html#5715" class="Bound">A</a>

  <a id="downFrom"></a><a id="5801" href="logbook.html#5801" class="Function">downFrom</a> <a id="5810" class="Symbol">:</a> <a id="5812" href="logbook.html#5678" class="Datatype">ℕ</a> <a id="5814" class="Symbol">→</a> <a id="5816" href="logbook.html#5705" class="Datatype">List</a> <a id="5821" href="logbook.html#5678" class="Datatype">ℕ</a>
  <a id="5825" href="logbook.html#5801" class="Function">downFrom</a> <a id="5834" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="5839" class="Symbol">=</a> <a id="5841" href="logbook.html#5744" class="InductiveConstructor">[]</a>
  <a id="5846" href="logbook.html#5801" class="Function">downFrom</a> <a id="5855" class="Symbol">(</a><a id="5856" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="5860" href="logbook.html#5860" class="Bound">n</a><a id="5861" class="Symbol">)</a> <a id="5863" class="Symbol">=</a> <a id="5865" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="5869" href="logbook.html#5860" class="Bound">n</a> <a id="5871" href="logbook.html#5761" class="InductiveConstructor Operator">∷</a> <a id="5873" href="logbook.html#5801" class="Function">downFrom</a> <a id="5882" href="logbook.html#5860" class="Bound">n</a> 
  
  <a id="sum"></a><a id="5890" href="logbook.html#5890" class="Function">sum</a> <a id="5894" class="Symbol">:</a> <a id="5896" href="logbook.html#5705" class="Datatype">List</a> <a id="5901" href="logbook.html#5678" class="Datatype">ℕ</a> <a id="5903" class="Symbol">→</a> <a id="5905" href="logbook.html#5678" class="Datatype">ℕ</a>
  <a id="5909" href="logbook.html#5890" class="Function">sum</a> <a id="5913" href="logbook.html#5744" class="InductiveConstructor">[]</a> <a id="5916" class="Symbol">=</a> <a id="5918" class="Number">0</a>
  <a id="5922" href="logbook.html#5890" class="Function">sum</a> <a id="5926" class="Symbol">(</a><a id="5927" href="logbook.html#5927" class="Bound">x</a> <a id="5929" href="logbook.html#5761" class="InductiveConstructor Operator">∷</a> <a id="5931" href="logbook.html#5931" class="Bound">xs</a><a id="5933" class="Symbol">)</a> <a id="5935" class="Symbol">=</a> <a id="5937" href="logbook.html#5927" class="Bound">x</a> <a id="5939" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="5941" href="logbook.html#5890" class="Function">sum</a> <a id="5945" href="logbook.html#5931" class="Bound">xs</a>
  
  <a id="main"></a><a id="5953" href="logbook.html#5953" class="Function">main</a> <a id="5958" class="Symbol">=</a> <a id="5960" href="logbook.html#5890" class="Function">sum</a> <a id="5964" class="Symbol">(</a><a id="5965" href="logbook.html#5801" class="Function">downFrom</a> <a id="5974" class="Number">4</a><a id="5975" class="Symbol">)</a>
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

