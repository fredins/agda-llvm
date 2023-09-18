---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

  <pre class="Agda">  <a id="89" class="Keyword">module</a> <a id="96" href="logbook.html" class="Module">logbook</a> <a id="104" class="Keyword">where</a>

  <a id="113" class="Keyword">open</a> <a id="118" class="Keyword">import</a> <a id="125" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="142" class="Keyword">using</a> <a id="148" class="Symbol">(</a><a id="149" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="152" class="Symbol">;</a> <a id="154" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="158" class="Symbol">;</a> <a id="160" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="163" class="Symbol">)</a> <a id="165" class="Keyword">renaming</a> <a id="174" class="Symbol">(</a><a id="175" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="179" class="Symbol">to</a> <a id="182" class="Datatype">ℕ</a><a id="183" class="Symbol">)</a> 

  <a id="189" class="Keyword">private</a> 
    <a id="202" class="Keyword">variable</a>
      <a id="217" href="logbook.html#217" class="Generalizable">A</a> <a id="219" href="logbook.html#219" class="Generalizable">B</a> <a id="221" class="Symbol">:</a> <a id="223" href="Agda.Primitive.html#320" class="Primitive">Set</a>

  <a id="230" class="Keyword">infixr</a> <a id="237" class="Number">5</a> <a id="239" href="logbook.html#290" class="InductiveConstructor Operator">_∷_</a>
  <a id="245" class="Keyword">data</a> <a id="List"></a><a id="250" href="logbook.html#250" class="Datatype">List</a> <a id="255" href="logbook.html#255" class="Bound">A</a> <a id="257" class="Symbol">:</a> <a id="259" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="263" class="Keyword">where</a>
    <a id="List.[]"></a><a id="273" href="logbook.html#273" class="InductiveConstructor">[]</a>  <a id="277" class="Symbol">:</a> <a id="279" href="logbook.html#250" class="Datatype">List</a> <a id="284" href="logbook.html#255" class="Bound">A</a>
    <a id="List._∷_"></a><a id="290" href="logbook.html#290" class="InductiveConstructor Operator">_∷_</a> <a id="294" class="Symbol">:</a> <a id="296" class="Symbol">(</a><a id="297" href="logbook.html#297" class="Bound">x</a> <a id="299" class="Symbol">:</a> <a id="301" href="logbook.html#255" class="Bound">A</a><a id="302" class="Symbol">)</a> <a id="304" class="Symbol">(</a><a id="305" href="logbook.html#305" class="Bound">xs</a> <a id="308" class="Symbol">:</a> <a id="310" href="logbook.html#250" class="Datatype">List</a> <a id="315" href="logbook.html#255" class="Bound">A</a><a id="316" class="Symbol">)</a> <a id="318" class="Symbol">→</a> <a id="320" href="logbook.html#250" class="Datatype">List</a> <a id="325" href="logbook.html#255" class="Bound">A</a>

</pre>

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
  

  <pre class="Agda">  <a id="1261" class="Keyword">module</a> <a id="DownFromTail"></a><a id="1268" href="logbook.html#1268" class="Module">DownFromTail</a> <a id="1281" class="Keyword">where</a>

    <a id="DownFromTail.downFrom"></a><a id="1292" href="logbook.html#1292" class="Function">downFrom</a> <a id="1301" class="Symbol">:</a> <a id="1303" href="logbook.html#250" class="Datatype">List</a> <a id="1308" href="logbook.html#182" class="Datatype">ℕ</a> <a id="1310" class="Symbol">→</a> <a id="1312" href="logbook.html#182" class="Datatype">ℕ</a> <a id="1314" class="Symbol">→</a> <a id="1316" href="logbook.html#250" class="Datatype">List</a> <a id="1321" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="1327" href="logbook.html#1292" class="Function">downFrom</a> <a id="1336" href="logbook.html#1336" class="Bound">acc</a> <a id="1340" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a>    <a id="1348" class="Symbol">=</a> <a id="1350" href="logbook.html#1336" class="Bound">acc</a>
    <a id="1358" href="logbook.html#1292" class="Function">downFrom</a> <a id="1367" href="logbook.html#1367" class="Bound">acc</a> <a id="1371" class="Symbol">(</a><a id="1372" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="1376" href="logbook.html#1376" class="Bound">n</a><a id="1377" class="Symbol">)</a> <a id="1379" class="Symbol">=</a> <a id="1381" href="logbook.html#1292" class="Function">downFrom</a> <a id="1390" class="Symbol">(</a><a id="1391" href="logbook.html#1376" class="Bound">n</a> <a id="1393" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="1395" href="logbook.html#1367" class="Bound">acc</a><a id="1398" class="Symbol">)</a> <a id="1400" href="logbook.html#1376" class="Bound">n</a>

    <a id="DownFromTail.sum"></a><a id="1407" href="logbook.html#1407" class="Function">sum</a> <a id="1411" class="Symbol">:</a> <a id="1413" href="logbook.html#182" class="Datatype">ℕ</a> <a id="1415" class="Symbol">→</a> <a id="1417" href="logbook.html#250" class="Datatype">List</a> <a id="1422" href="logbook.html#182" class="Datatype">ℕ</a> <a id="1424" class="Symbol">→</a> <a id="1426" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="1432" href="logbook.html#1407" class="Function">sum</a> <a id="1436" href="logbook.html#1436" class="Bound">acc</a> <a id="1440" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="1443" class="Symbol">=</a> <a id="1445" href="logbook.html#1436" class="Bound">acc</a>
    <a id="1453" href="logbook.html#1407" class="Function">sum</a> <a id="1457" href="logbook.html#1457" class="Bound">acc</a> <a id="1461" class="Symbol">(</a><a id="1462" href="logbook.html#1462" class="Bound">x</a> <a id="1464" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="1466" href="logbook.html#1466" class="Bound">xs</a><a id="1468" class="Symbol">)</a> <a id="1470" class="Symbol">=</a> <a id="1472" href="logbook.html#1407" class="Function">sum</a> <a id="1476" class="Symbol">(</a><a id="1477" href="logbook.html#1462" class="Bound">x</a> <a id="1479" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="1481" href="logbook.html#1457" class="Bound">acc</a><a id="1484" class="Symbol">)</a> <a id="1486" href="logbook.html#1466" class="Bound">xs</a>

    <a id="1494" class="Comment">-- Current max: 74 000</a>
    <a id="DownFromTail.main"></a><a id="1521" href="logbook.html#1521" class="Function">main</a> <a id="1526" class="Symbol">=</a> <a id="1528" href="logbook.html#1407" class="Function">sum</a> <a id="1532" class="Number">0</a> <a id="1534" class="Symbol">(</a><a id="1535" href="logbook.html#1292" class="Function">downFrom</a> <a id="1544" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="1547" class="Number">100</a><a id="1550" class="Symbol">)</a>
</pre>
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

  <pre class="Agda">  <a id="7609" class="Keyword">module</a> <a id="Example"></a><a id="7616" href="logbook.html#7616" class="Module">Example</a> <a id="7624" class="Keyword">where</a>

    <a id="Example.downFrom"></a><a id="7635" href="logbook.html#7635" class="Function">downFrom</a> <a id="7644" class="Symbol">:</a> <a id="7646" href="logbook.html#182" class="Datatype">ℕ</a> <a id="7648" class="Symbol">→</a> <a id="7650" href="logbook.html#250" class="Datatype">List</a> <a id="7655" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="7661" href="logbook.html#7635" class="Function">downFrom</a> <a id="7670" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a>    <a id="7678" class="Symbol">=</a> <a id="7680" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="7687" href="logbook.html#7635" class="Function">downFrom</a> <a id="7696" class="Symbol">(</a><a id="7697" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="7701" href="logbook.html#7701" class="Bound">n</a><a id="7702" class="Symbol">)</a> <a id="7704" class="Symbol">=</a> <a id="7706" href="logbook.html#7701" class="Bound">n</a> <a id="7708" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="7710" href="logbook.html#7635" class="Function">downFrom</a> <a id="7719" href="logbook.html#7701" class="Bound">n</a> 

    <a id="Example.mapDouble"></a><a id="7727" href="logbook.html#7727" class="Function">mapDouble</a> <a id="7737" class="Symbol">:</a> <a id="7739" href="logbook.html#250" class="Datatype">List</a> <a id="7744" href="logbook.html#182" class="Datatype">ℕ</a> <a id="7746" class="Symbol">→</a> <a id="7748" href="logbook.html#250" class="Datatype">List</a> <a id="7753" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="7759" href="logbook.html#7727" class="Function">mapDouble</a> <a id="7769" href="logbook.html#273" class="InductiveConstructor">[]</a>       <a id="7778" class="Symbol">=</a> <a id="7780" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="7787" href="logbook.html#7727" class="Function">mapDouble</a> <a id="7797" class="Symbol">(</a><a id="7798" href="logbook.html#7798" class="Bound">x</a> <a id="7800" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="7802" href="logbook.html#7802" class="Bound">xs</a><a id="7804" class="Symbol">)</a> <a id="7806" class="Symbol">=</a> <a id="7808" href="logbook.html#7798" class="Bound">x</a> <a id="7810" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="7812" href="logbook.html#7798" class="Bound">x</a> <a id="7814" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="7816" href="logbook.html#7727" class="Function">mapDouble</a> <a id="7826" href="logbook.html#7802" class="Bound">xs</a>

    <a id="Example.sum"></a><a id="7834" href="logbook.html#7834" class="Function">sum</a> <a id="7838" class="Symbol">:</a> <a id="7840" href="logbook.html#250" class="Datatype">List</a> <a id="7845" href="logbook.html#182" class="Datatype">ℕ</a> <a id="7847" class="Symbol">→</a> <a id="7849" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="7855" href="logbook.html#7834" class="Function">sum</a> <a id="7859" href="logbook.html#273" class="InductiveConstructor">[]</a>       <a id="7868" class="Symbol">=</a> <a id="7870" class="Number">0</a>
    <a id="7876" href="logbook.html#7834" class="Function">sum</a> <a id="7880" class="Symbol">(</a><a id="7881" href="logbook.html#7881" class="Bound">x</a> <a id="7883" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="7885" href="logbook.html#7885" class="Bound">xs</a><a id="7887" class="Symbol">)</a> <a id="7889" class="Symbol">=</a> <a id="7891" href="logbook.html#7881" class="Bound">x</a> <a id="7893" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="7895" href="logbook.html#7834" class="Function">sum</a> <a id="7899" href="logbook.html#7885" class="Bound">xs</a>

    <a id="Example.main"></a><a id="7907" href="logbook.html#7907" class="Function">main</a> <a id="7912" class="Symbol">=</a> <a id="7914" href="logbook.html#7834" class="Function">sum</a> <a id="7918" class="Symbol">(</a><a id="7919" href="logbook.html#7727" class="Function">mapDouble</a> <a id="7929" class="Symbol">(</a><a id="7930" href="logbook.html#7635" class="Function">downFrom</a> <a id="7939" class="Number">10000</a><a id="7944" class="Symbol">))</a> 
</pre>
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

  <pre class="Agda">  <a id="17753" class="Keyword">module</a> <a id="DownFrom"></a><a id="17760" href="logbook.html#17760" class="Module">DownFrom</a> <a id="17769" class="Keyword">where</a>

    <a id="DownFrom.downFrom"></a><a id="17780" href="logbook.html#17780" class="Function">downFrom</a> <a id="17789" class="Symbol">:</a> <a id="17791" href="logbook.html#182" class="Datatype">ℕ</a> <a id="17793" class="Symbol">→</a> <a id="17795" href="logbook.html#250" class="Datatype">List</a> <a id="17800" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="17806" href="logbook.html#17780" class="Function">downFrom</a> <a id="17815" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="17820" class="Symbol">=</a> <a id="17822" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="17829" href="logbook.html#17780" class="Function">downFrom</a> <a id="17838" class="Symbol">(</a><a id="17839" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="17843" href="logbook.html#17843" class="Bound">n</a><a id="17844" class="Symbol">)</a> <a id="17846" class="Symbol">=</a> <a id="17848" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="17852" href="logbook.html#17843" class="Bound">n</a> <a id="17854" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="17856" href="logbook.html#17780" class="Function">downFrom</a> <a id="17865" href="logbook.html#17843" class="Bound">n</a> 

    <a id="DownFrom.sum"></a><a id="17873" href="logbook.html#17873" class="Function">sum</a> <a id="17877" class="Symbol">:</a> <a id="17879" href="logbook.html#250" class="Datatype">List</a> <a id="17884" href="logbook.html#182" class="Datatype">ℕ</a> <a id="17886" class="Symbol">→</a> <a id="17888" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="17894" href="logbook.html#17873" class="Function">sum</a> <a id="17898" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="17901" class="Symbol">=</a> <a id="17903" class="Number">0</a>
    <a id="17909" href="logbook.html#17873" class="Function">sum</a> <a id="17913" class="Symbol">(</a><a id="17914" href="logbook.html#17914" class="Bound">x</a> <a id="17916" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="17918" href="logbook.html#17918" class="Bound">xs</a><a id="17920" class="Symbol">)</a> <a id="17922" class="Symbol">=</a> <a id="17924" href="logbook.html#17914" class="Bound">x</a> <a id="17926" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="17928" href="logbook.html#17873" class="Function">sum</a> <a id="17932" href="logbook.html#17918" class="Bound">xs</a>

    <a id="DownFrom.main"></a><a id="17940" href="logbook.html#17940" class="Function">main</a> <a id="17945" class="Symbol">=</a> <a id="17947" href="logbook.html#17873" class="Function">sum</a> <a id="17951" class="Symbol">(</a><a id="17952" href="logbook.html#17780" class="Function">downFrom</a> <a id="17961" class="Number">4</a><a id="17962" class="Symbol">)</a>
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

