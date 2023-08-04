---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---


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
  <a id="7004" class="Keyword">open</a> <a id="7009" class="Keyword">import</a> <a id="7016" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="7033" class="Keyword">using</a> <a id="7039" class="Symbol">(</a><a id="7040" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="7043" class="Symbol">;</a> <a id="7045" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="7049" class="Symbol">;</a> <a id="7051" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="7054" class="Symbol">)</a> <a id="7056" class="Keyword">renaming</a> <a id="7065" class="Symbol">(</a><a id="7066" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="7070" class="Symbol">to</a> <a id="7073" class="Datatype">ℕ</a><a id="7074" class="Symbol">)</a> 

  <a id="7080" class="Keyword">infixr</a> <a id="7087" class="Number">5</a> <a id="7089" href="logbook.html#7156" class="InductiveConstructor Operator">_∷_</a>
  <a id="7095" class="Keyword">data</a> <a id="List"></a><a id="7100" href="logbook.html#7100" class="Datatype">List</a> <a id="7105" class="Symbol">{</a><a id="7106" href="logbook.html#7106" class="Bound">a</a><a id="7107" class="Symbol">}</a> <a id="7109" class="Symbol">(</a><a id="7110" href="logbook.html#7110" class="Bound">A</a> <a id="7112" class="Symbol">:</a> <a id="7114" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="7118" href="logbook.html#7106" class="Bound">a</a><a id="7119" class="Symbol">)</a> <a id="7121" class="Symbol">:</a> <a id="7123" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="7127" href="logbook.html#7106" class="Bound">a</a> <a id="7129" class="Keyword">where</a>
    <a id="List.[]"></a><a id="7139" href="logbook.html#7139" class="InductiveConstructor">[]</a>  <a id="7143" class="Symbol">:</a> <a id="7145" href="logbook.html#7100" class="Datatype">List</a> <a id="7150" href="logbook.html#7110" class="Bound">A</a>
    <a id="List._∷_"></a><a id="7156" href="logbook.html#7156" class="InductiveConstructor Operator">_∷_</a> <a id="7160" class="Symbol">:</a> <a id="7162" class="Symbol">(</a><a id="7163" href="logbook.html#7163" class="Bound">x</a> <a id="7165" class="Symbol">:</a> <a id="7167" href="logbook.html#7110" class="Bound">A</a><a id="7168" class="Symbol">)</a> <a id="7170" class="Symbol">(</a><a id="7171" href="logbook.html#7171" class="Bound">xs</a> <a id="7174" class="Symbol">:</a> <a id="7176" href="logbook.html#7100" class="Datatype">List</a> <a id="7181" href="logbook.html#7110" class="Bound">A</a><a id="7182" class="Symbol">)</a> <a id="7184" class="Symbol">→</a> <a id="7186" href="logbook.html#7100" class="Datatype">List</a> <a id="7191" href="logbook.html#7110" class="Bound">A</a>

  <a id="downFrom"></a><a id="7196" href="logbook.html#7196" class="Function">downFrom</a> <a id="7205" class="Symbol">:</a> <a id="7207" href="logbook.html#7073" class="Datatype">ℕ</a> <a id="7209" class="Symbol">→</a> <a id="7211" href="logbook.html#7100" class="Datatype">List</a> <a id="7216" href="logbook.html#7073" class="Datatype">ℕ</a>
  <a id="7220" href="logbook.html#7196" class="Function">downFrom</a> <a id="7229" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="7234" class="Symbol">=</a> <a id="7236" href="logbook.html#7139" class="InductiveConstructor">[]</a>
  <a id="7241" href="logbook.html#7196" class="Function">downFrom</a> <a id="7250" class="Symbol">(</a><a id="7251" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="7255" href="logbook.html#7255" class="Bound">n</a><a id="7256" class="Symbol">)</a> <a id="7258" class="Symbol">=</a> <a id="7260" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="7264" href="logbook.html#7255" class="Bound">n</a> <a id="7266" href="logbook.html#7156" class="InductiveConstructor Operator">∷</a> <a id="7268" href="logbook.html#7196" class="Function">downFrom</a> <a id="7277" href="logbook.html#7255" class="Bound">n</a> 
  
  <a id="sum"></a><a id="7285" href="logbook.html#7285" class="Function">sum</a> <a id="7289" class="Symbol">:</a> <a id="7291" href="logbook.html#7100" class="Datatype">List</a> <a id="7296" href="logbook.html#7073" class="Datatype">ℕ</a> <a id="7298" class="Symbol">→</a> <a id="7300" href="logbook.html#7073" class="Datatype">ℕ</a>
  <a id="7304" href="logbook.html#7285" class="Function">sum</a> <a id="7308" href="logbook.html#7139" class="InductiveConstructor">[]</a> <a id="7311" class="Symbol">=</a> <a id="7313" class="Number">0</a>
  <a id="7317" href="logbook.html#7285" class="Function">sum</a> <a id="7321" class="Symbol">(</a><a id="7322" href="logbook.html#7322" class="Bound">x</a> <a id="7324" href="logbook.html#7156" class="InductiveConstructor Operator">∷</a> <a id="7326" href="logbook.html#7326" class="Bound">xs</a><a id="7328" class="Symbol">)</a> <a id="7330" class="Symbol">=</a> <a id="7332" href="logbook.html#7322" class="Bound">x</a> <a id="7334" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="7336" href="logbook.html#7285" class="Function">sum</a> <a id="7340" href="logbook.html#7326" class="Bound">xs</a>
  
  <a id="main"></a><a id="7348" href="logbook.html#7348" class="Function">main</a> <a id="7353" class="Symbol">=</a> <a id="7355" href="logbook.html#7285" class="Function">sum</a> <a id="7359" class="Symbol">(</a><a id="7360" href="logbook.html#7196" class="Function">downFrom</a> <a id="7369" class="Number">4</a><a id="7370" class="Symbol">)</a>
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

