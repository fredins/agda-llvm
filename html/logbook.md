---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

### W. 29

Read the following papers:

- @boquist1996

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
  <a id="5169" class="Keyword">open</a> <a id="5174" class="Keyword">import</a> <a id="5181" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="5198" class="Keyword">using</a> <a id="5204" class="Symbol">(</a><a id="5205" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="5208" class="Symbol">;</a> <a id="5210" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="5214" class="Symbol">;</a> <a id="5216" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="5219" class="Symbol">)</a> <a id="5221" class="Keyword">renaming</a> <a id="5230" class="Symbol">(</a><a id="5231" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="5235" class="Symbol">to</a> <a id="5238" class="Datatype">ℕ</a><a id="5239" class="Symbol">)</a> 

  <a id="5245" class="Keyword">infixr</a> <a id="5252" class="Number">5</a> <a id="5254" href="logbook.html#5321" class="InductiveConstructor Operator">_∷_</a>
  <a id="5260" class="Keyword">data</a> <a id="List"></a><a id="5265" href="logbook.html#5265" class="Datatype">List</a> <a id="5270" class="Symbol">{</a><a id="5271" href="logbook.html#5271" class="Bound">a</a><a id="5272" class="Symbol">}</a> <a id="5274" class="Symbol">(</a><a id="5275" href="logbook.html#5275" class="Bound">A</a> <a id="5277" class="Symbol">:</a> <a id="5279" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="5283" href="logbook.html#5271" class="Bound">a</a><a id="5284" class="Symbol">)</a> <a id="5286" class="Symbol">:</a> <a id="5288" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="5292" href="logbook.html#5271" class="Bound">a</a> <a id="5294" class="Keyword">where</a>
    <a id="List.[]"></a><a id="5304" href="logbook.html#5304" class="InductiveConstructor">[]</a>  <a id="5308" class="Symbol">:</a> <a id="5310" href="logbook.html#5265" class="Datatype">List</a> <a id="5315" href="logbook.html#5275" class="Bound">A</a>
    <a id="List._∷_"></a><a id="5321" href="logbook.html#5321" class="InductiveConstructor Operator">_∷_</a> <a id="5325" class="Symbol">:</a> <a id="5327" class="Symbol">(</a><a id="5328" href="logbook.html#5328" class="Bound">x</a> <a id="5330" class="Symbol">:</a> <a id="5332" href="logbook.html#5275" class="Bound">A</a><a id="5333" class="Symbol">)</a> <a id="5335" class="Symbol">(</a><a id="5336" href="logbook.html#5336" class="Bound">xs</a> <a id="5339" class="Symbol">:</a> <a id="5341" href="logbook.html#5265" class="Datatype">List</a> <a id="5346" href="logbook.html#5275" class="Bound">A</a><a id="5347" class="Symbol">)</a> <a id="5349" class="Symbol">→</a> <a id="5351" href="logbook.html#5265" class="Datatype">List</a> <a id="5356" href="logbook.html#5275" class="Bound">A</a>

  <a id="downFrom"></a><a id="5361" href="logbook.html#5361" class="Function">downFrom</a> <a id="5370" class="Symbol">:</a> <a id="5372" href="logbook.html#5238" class="Datatype">ℕ</a> <a id="5374" class="Symbol">→</a> <a id="5376" href="logbook.html#5265" class="Datatype">List</a> <a id="5381" href="logbook.html#5238" class="Datatype">ℕ</a>
  <a id="5385" href="logbook.html#5361" class="Function">downFrom</a> <a id="5394" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="5399" class="Symbol">=</a> <a id="5401" href="logbook.html#5304" class="InductiveConstructor">[]</a>
  <a id="5406" href="logbook.html#5361" class="Function">downFrom</a> <a id="5415" class="Symbol">(</a><a id="5416" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="5420" href="logbook.html#5420" class="Bound">n</a><a id="5421" class="Symbol">)</a> <a id="5423" class="Symbol">=</a> <a id="5425" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="5429" href="logbook.html#5420" class="Bound">n</a> <a id="5431" href="logbook.html#5321" class="InductiveConstructor Operator">∷</a> <a id="5433" href="logbook.html#5361" class="Function">downFrom</a> <a id="5442" href="logbook.html#5420" class="Bound">n</a> 
  
  <a id="sum"></a><a id="5450" href="logbook.html#5450" class="Function">sum</a> <a id="5454" class="Symbol">:</a> <a id="5456" href="logbook.html#5265" class="Datatype">List</a> <a id="5461" href="logbook.html#5238" class="Datatype">ℕ</a> <a id="5463" class="Symbol">→</a> <a id="5465" href="logbook.html#5238" class="Datatype">ℕ</a>
  <a id="5469" href="logbook.html#5450" class="Function">sum</a> <a id="5473" href="logbook.html#5304" class="InductiveConstructor">[]</a> <a id="5476" class="Symbol">=</a> <a id="5478" class="Number">0</a>
  <a id="5482" href="logbook.html#5450" class="Function">sum</a> <a id="5486" class="Symbol">(</a><a id="5487" href="logbook.html#5487" class="Bound">x</a> <a id="5489" href="logbook.html#5321" class="InductiveConstructor Operator">∷</a> <a id="5491" href="logbook.html#5491" class="Bound">xs</a><a id="5493" class="Symbol">)</a> <a id="5495" class="Symbol">=</a> <a id="5497" href="logbook.html#5487" class="Bound">x</a> <a id="5499" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="5501" href="logbook.html#5450" class="Function">sum</a> <a id="5505" href="logbook.html#5491" class="Bound">xs</a>
  
  <a id="main"></a><a id="5513" href="logbook.html#5513" class="Function">main</a> <a id="5518" class="Symbol">=</a> <a id="5520" href="logbook.html#5450" class="Function">sum</a> <a id="5524" class="Symbol">(</a><a id="5525" href="logbook.html#5361" class="Function">downFrom</a> <a id="5534" class="Number">4</a><a id="5535" class="Symbol">)</a>
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

References
----------

