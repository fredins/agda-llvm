---
bibliography: bibliography.bib
csl: acm-siggraph.csl
css: Agda.css
---

  <pre class="Agda">  <a id="89" class="Keyword">module</a> <a id="96" href="logbook.html" class="Module">logbook</a> <a id="104" class="Keyword">where</a>

  <a id="113" class="Keyword">open</a> <a id="118" class="Keyword">import</a> <a id="125" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="142" class="Keyword">using</a> <a id="148" class="Symbol">(</a><a id="149" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a><a id="152" class="Symbol">;</a> <a id="154" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a><a id="158" class="Symbol">;</a> <a id="160" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">_+_</a><a id="163" class="Symbol">)</a> <a id="165" class="Keyword">renaming</a> <a id="174" class="Symbol">(</a><a id="175" href="Agda.Builtin.Nat.html#203" class="Datatype">Nat</a> <a id="179" class="Symbol">to</a> <a id="182" class="Datatype">ℕ</a><a id="183" class="Symbol">)</a> 

  <a id="189" class="Keyword">private</a> 
    <a id="202" class="Keyword">variable</a>
      <a id="217" href="logbook.html#217" class="Generalizable">A</a> <a id="219" href="logbook.html#219" class="Generalizable">B</a> <a id="221" class="Symbol">:</a> <a id="223" href="Agda.Primitive.html#388" class="Primitive">Set</a>

  <a id="230" class="Keyword">infixr</a> <a id="237" class="Number">5</a> <a id="239" href="logbook.html#290" class="InductiveConstructor Operator">_∷_</a>
  <a id="245" class="Keyword">data</a> <a id="List"></a><a id="250" href="logbook.html#250" class="Datatype">List</a> <a id="255" href="logbook.html#255" class="Bound">A</a> <a id="257" class="Symbol">:</a> <a id="259" href="Agda.Primitive.html#388" class="Primitive">Set</a> <a id="263" class="Keyword">where</a>
    <a id="List.[]"></a><a id="273" href="logbook.html#273" class="InductiveConstructor">[]</a>  <a id="277" class="Symbol">:</a> <a id="279" href="logbook.html#250" class="Datatype">List</a> <a id="284" href="logbook.html#255" class="Bound">A</a>
    <a id="List._∷_"></a><a id="290" href="logbook.html#290" class="InductiveConstructor Operator">_∷_</a> <a id="294" class="Symbol">:</a> <a id="296" href="logbook.html#255" class="Bound">A</a> <a id="298" class="Symbol">→</a> <a id="300" href="logbook.html#250" class="Datatype">List</a> <a id="305" href="logbook.html#255" class="Bound">A</a> <a id="307" class="Symbol">→</a> <a id="309" href="logbook.html#250" class="Datatype">List</a> <a id="314" href="logbook.html#255" class="Bound">A</a>

</pre>
### W.3 W.4


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

  <pre class="Agda">  <a id="4016" class="Keyword">module</a> <a id="DownFromOpt"></a><a id="4023" href="logbook.html#4023" class="Module">DownFromOpt</a> <a id="4035" class="Keyword">where</a>
    <a id="4045" class="Keyword">open</a> <a id="4050" class="Keyword">import</a> <a id="4057" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="4074" class="Keyword">using</a> <a id="4080" class="Symbol">(</a><a id="4081" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a><a id="4084" class="Symbol">;</a> <a id="4086" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a><a id="4090" class="Symbol">;</a> <a id="4092" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">_+_</a><a id="4095" class="Symbol">)</a> <a id="4097" class="Keyword">renaming</a> <a id="4106" class="Symbol">(</a><a id="4107" href="Agda.Builtin.Nat.html#203" class="Datatype">Nat</a> <a id="4111" class="Symbol">to</a> <a id="4114" class="Datatype">ℕ</a><a id="4115" class="Symbol">)</a> 
    <a id="4122" class="Keyword">open</a> <a id="4127" class="Keyword">import</a> <a id="4134" href="Agda.Builtin.Strict.html" class="Module">Agda.Builtin.Strict</a> <a id="4154" class="Keyword">using</a> <a id="4160" class="Symbol">(</a><a id="4161" href="Agda.Builtin.Strict.html#178" class="Primitive">primForce</a><a id="4170" class="Symbol">)</a>

    <a id="4177" class="Symbol">{-#</a> <a id="4181" class="Keyword">TERMINATING</a> <a id="4193" class="Symbol">#-}</a>
    <a id="DownFromOpt.downFrom"></a><a id="4201" href="logbook.html#4201" class="Function">downFrom</a> <a id="4210" class="Symbol">:</a> <a id="4212" href="logbook.html#4114" class="Datatype">ℕ</a> <a id="4214" class="Symbol">→</a> <a id="4216" href="logbook.html#250" class="Datatype">List</a> <a id="4221" href="logbook.html#4114" class="Datatype">ℕ</a>
    <a id="4227" href="logbook.html#4201" class="Function">downFrom</a> <a id="4236" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a> <a id="4241" class="Symbol">=</a> <a id="4243" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="4250" href="logbook.html#4201" class="Function">downFrom</a> <a id="4259" class="Symbol">(</a><a id="4260" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="4264" href="logbook.html#4264" class="Bound">n</a><a id="4265" class="Symbol">)</a> <a id="4267" class="Symbol">=</a> <a id="4269" href="Agda.Builtin.Strict.html#178" class="Primitive">primForce</a> <a id="4279" href="logbook.html#4264" class="Bound">n</a> <a id="4281" class="Symbol">(λ</a> <a id="4284" href="logbook.html#4284" class="Bound">n</a> <a id="4286" class="Symbol">→</a> <a id="4288" href="logbook.html#4284" class="Bound">n</a> <a id="4290" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="4292" href="logbook.html#4201" class="Function">downFrom</a> <a id="4301" href="logbook.html#4284" class="Bound">n</a><a id="4302" class="Symbol">)</a>

    <a id="DownFromOpt.sum"></a><a id="4309" href="logbook.html#4309" class="Function">sum</a> <a id="4313" class="Symbol">:</a> <a id="4315" href="logbook.html#4114" class="Datatype">ℕ</a> <a id="4317" class="Symbol">→</a> <a id="4319" href="logbook.html#250" class="Datatype">List</a> <a id="4324" href="logbook.html#4114" class="Datatype">ℕ</a> <a id="4326" class="Symbol">→</a> <a id="4328" href="logbook.html#4114" class="Datatype">ℕ</a>
    <a id="4334" href="logbook.html#4309" class="Function">sum</a> <a id="4338" href="logbook.html#4338" class="Bound">acc</a> <a id="4342" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="4345" class="Symbol">=</a> <a id="4347" href="logbook.html#4338" class="Bound">acc</a>
    <a id="4355" href="logbook.html#4309" class="Function">sum</a> <a id="4359" href="logbook.html#4359" class="Bound">acc</a> <a id="4363" class="Symbol">(</a><a id="4364" href="logbook.html#4364" class="Bound">x</a> <a id="4366" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="4368" href="logbook.html#4368" class="Bound">xs</a><a id="4370" class="Symbol">)</a> <a id="4372" class="Symbol">=</a> <a id="4374" href="logbook.html#4309" class="Function">sum</a> <a id="4378" class="Symbol">(</a><a id="4379" href="Agda.Builtin.Strict.html#178" class="Primitive">primForce</a> <a id="4389" href="logbook.html#4364" class="Bound">x</a> <a id="4391" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">_+_</a> <a id="4395" href="logbook.html#4359" class="Bound">acc</a><a id="4398" class="Symbol">)</a> <a id="4400" href="logbook.html#4368" class="Bound">xs</a>

    <a id="DownFromOpt.main"></a><a id="4408" href="logbook.html#4408" class="Function">main</a> <a id="4413" class="Symbol">=</a> <a id="4415" href="logbook.html#4309" class="Function">sum</a> <a id="4419" class="Number">0</a> <a id="4421" class="Symbol">(</a><a id="4422" href="logbook.html#4201" class="Function">downFrom</a> <a id="4431" class="Number">100</a><a id="4434" class="Symbol">)</a> 
</pre>
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

  <pre class="Agda">  <a id="5946" class="Keyword">module</a> <a id="DownFromTail′"></a><a id="5953" href="logbook.html#5953" class="Module">DownFromTail′</a> <a id="5967" class="Keyword">where</a>

    <a id="5978" class="Keyword">open</a> <a id="5983" class="Keyword">import</a> <a id="5990" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="6007" class="Keyword">using</a> <a id="6013" class="Symbol">(</a><a id="6014" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a><a id="6017" class="Symbol">;</a> <a id="6019" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a><a id="6023" class="Symbol">;</a> <a id="6025" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">_+_</a><a id="6028" class="Symbol">)</a> <a id="6030" class="Keyword">renaming</a> <a id="6039" class="Symbol">(</a><a id="6040" href="Agda.Builtin.Nat.html#203" class="Datatype">Nat</a> <a id="6044" class="Symbol">to</a> <a id="6047" class="Datatype">ℕ</a><a id="6048" class="Symbol">)</a> 
    <a id="6055" class="Keyword">open</a> <a id="6060" class="Keyword">import</a> <a id="6067" href="Agda.Builtin.Strict.html" class="Module">Agda.Builtin.Strict</a> <a id="6087" class="Keyword">using</a> <a id="6093" class="Symbol">(</a><a id="6094" href="Agda.Builtin.Strict.html#178" class="Primitive">primForce</a><a id="6103" class="Symbol">)</a>

    <a id="DownFromTail′.downFrom"></a><a id="6110" href="logbook.html#6110" class="Function">downFrom</a> <a id="6119" class="Symbol">:</a> <a id="6121" href="logbook.html#250" class="Datatype">List</a> <a id="6126" href="logbook.html#6047" class="Datatype">ℕ</a> <a id="6128" class="Symbol">→</a> <a id="6130" href="logbook.html#6047" class="Datatype">ℕ</a> <a id="6132" class="Symbol">→</a> <a id="6134" href="logbook.html#250" class="Datatype">List</a> <a id="6139" href="logbook.html#6047" class="Datatype">ℕ</a>
    <a id="6145" href="logbook.html#6110" class="Function">downFrom</a> <a id="6154" href="logbook.html#6154" class="Bound">acc</a> <a id="6158" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a>    <a id="6166" class="Symbol">=</a> <a id="6168" href="logbook.html#6154" class="Bound">acc</a>
    <a id="6176" href="logbook.html#6110" class="Function">downFrom</a> <a id="6185" href="logbook.html#6185" class="Bound">acc</a> <a id="6189" class="Symbol">(</a><a id="6190" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="6194" href="logbook.html#6194" class="Bound">n</a><a id="6195" class="Symbol">)</a> <a id="6197" class="Symbol">=</a> <a id="6199" href="logbook.html#6110" class="Function">downFrom</a> <a id="6208" class="Symbol">(</a><a id="6209" href="logbook.html#6194" class="Bound">n</a> <a id="6211" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="6213" href="logbook.html#6185" class="Bound">acc</a><a id="6216" class="Symbol">)</a> <a id="6218" href="logbook.html#6194" class="Bound">n</a>

    <a id="DownFromTail′.sum"></a><a id="6225" href="logbook.html#6225" class="Function">sum</a> <a id="6229" class="Symbol">:</a> <a id="6231" href="logbook.html#6047" class="Datatype">ℕ</a> <a id="6233" class="Symbol">→</a> <a id="6235" href="logbook.html#250" class="Datatype">List</a> <a id="6240" href="logbook.html#6047" class="Datatype">ℕ</a> <a id="6242" class="Symbol">→</a> <a id="6244" href="logbook.html#6047" class="Datatype">ℕ</a>
    <a id="6250" href="logbook.html#6225" class="Function">sum</a> <a id="6254" href="logbook.html#6254" class="Bound">acc</a> <a id="6258" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="6261" class="Symbol">=</a> <a id="6263" href="logbook.html#6254" class="Bound">acc</a>
    <a id="6271" href="logbook.html#6225" class="Function">sum</a> <a id="6275" href="logbook.html#6275" class="Bound">acc</a> <a id="6279" class="Symbol">(</a><a id="6280" href="logbook.html#6280" class="Bound">x</a> <a id="6282" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="6284" href="logbook.html#6284" class="Bound">xs</a><a id="6286" class="Symbol">)</a> <a id="6288" class="Symbol">=</a> <a id="6290" href="logbook.html#6225" class="Function">sum</a> <a id="6294" class="Symbol">(</a><a id="6295" href="Agda.Builtin.Strict.html#178" class="Primitive">primForce</a> <a id="6305" href="logbook.html#6280" class="Bound">x</a> <a id="6307" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">_+_</a> <a id="6311" href="logbook.html#6275" class="Bound">acc</a><a id="6314" class="Symbol">)</a> <a id="6316" href="logbook.html#6284" class="Bound">xs</a>

    <a id="6324" class="Comment">-- Your computer&#39;s memory is the limit!</a>
    <a id="DownFromTail′.main"></a><a id="6368" href="logbook.html#6368" class="Function">main</a> <a id="6373" class="Symbol">=</a> <a id="6375" href="logbook.html#6225" class="Function">sum</a> <a id="6379" class="Number">0</a> <a id="6381" class="Symbol">(</a><a id="6382" href="logbook.html#6110" class="Function">downFrom</a> <a id="6391" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="6394" class="Number">10_000_000</a><a id="6404" class="Symbol">)</a>
</pre>- I have updated the Github README somewhat.

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
  

  <pre class="Agda">  <a id="8013" class="Keyword">module</a> <a id="DownFromTail"></a><a id="8020" href="logbook.html#8020" class="Module">DownFromTail</a> <a id="8033" class="Keyword">where</a>

    <a id="DownFromTail.downFrom"></a><a id="8044" href="logbook.html#8044" class="Function">downFrom</a> <a id="8053" class="Symbol">:</a> <a id="8055" href="logbook.html#250" class="Datatype">List</a> <a id="8060" href="logbook.html#182" class="Datatype">ℕ</a> <a id="8062" class="Symbol">→</a> <a id="8064" href="logbook.html#182" class="Datatype">ℕ</a> <a id="8066" class="Symbol">→</a> <a id="8068" href="logbook.html#250" class="Datatype">List</a> <a id="8073" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="8079" href="logbook.html#8044" class="Function">downFrom</a> <a id="8088" href="logbook.html#8088" class="Bound">acc</a> <a id="8092" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a>    <a id="8100" class="Symbol">=</a> <a id="8102" href="logbook.html#8088" class="Bound">acc</a>
    <a id="8110" href="logbook.html#8044" class="Function">downFrom</a> <a id="8119" href="logbook.html#8119" class="Bound">acc</a> <a id="8123" class="Symbol">(</a><a id="8124" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="8128" href="logbook.html#8128" class="Bound">n</a><a id="8129" class="Symbol">)</a> <a id="8131" class="Symbol">=</a> <a id="8133" href="logbook.html#8044" class="Function">downFrom</a> <a id="8142" class="Symbol">(</a><a id="8143" href="logbook.html#8128" class="Bound">n</a> <a id="8145" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="8147" href="logbook.html#8119" class="Bound">acc</a><a id="8150" class="Symbol">)</a> <a id="8152" href="logbook.html#8128" class="Bound">n</a>

    <a id="DownFromTail.sum"></a><a id="8159" href="logbook.html#8159" class="Function">sum</a> <a id="8163" class="Symbol">:</a> <a id="8165" href="logbook.html#182" class="Datatype">ℕ</a> <a id="8167" class="Symbol">→</a> <a id="8169" href="logbook.html#250" class="Datatype">List</a> <a id="8174" href="logbook.html#182" class="Datatype">ℕ</a> <a id="8176" class="Symbol">→</a> <a id="8178" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="8184" href="logbook.html#8159" class="Function">sum</a> <a id="8188" href="logbook.html#8188" class="Bound">acc</a> <a id="8192" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="8195" class="Symbol">=</a> <a id="8197" href="logbook.html#8188" class="Bound">acc</a>
    <a id="8205" href="logbook.html#8159" class="Function">sum</a> <a id="8209" href="logbook.html#8209" class="Bound">acc</a> <a id="8213" class="Symbol">(</a><a id="8214" href="logbook.html#8214" class="Bound">x</a> <a id="8216" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="8218" href="logbook.html#8218" class="Bound">xs</a><a id="8220" class="Symbol">)</a> <a id="8222" class="Symbol">=</a> <a id="8224" href="logbook.html#8159" class="Function">sum</a> <a id="8228" class="Symbol">(</a><a id="8229" href="logbook.html#8214" class="Bound">x</a> <a id="8231" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">+</a> <a id="8233" href="logbook.html#8209" class="Bound">acc</a><a id="8236" class="Symbol">)</a> <a id="8238" href="logbook.html#8218" class="Bound">xs</a>

    <a id="8246" class="Comment">-- Current max: 74 000</a>
    <a id="DownFromTail.main"></a><a id="8273" href="logbook.html#8273" class="Function">main</a> <a id="8278" class="Symbol">=</a> <a id="8280" href="logbook.html#8159" class="Function">sum</a> <a id="8284" class="Number">0</a> <a id="8286" class="Symbol">(</a><a id="8287" href="logbook.html#8044" class="Function">downFrom</a> <a id="8296" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="8299" class="Number">100</a><a id="8302" class="Symbol">)</a>
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

  <pre class="Agda">  <a id="14361" class="Keyword">module</a> <a id="Example"></a><a id="14368" href="logbook.html#14368" class="Module">Example</a> <a id="14376" class="Keyword">where</a>

    <a id="Example.downFrom"></a><a id="14387" href="logbook.html#14387" class="Function">downFrom</a> <a id="14396" class="Symbol">:</a> <a id="14398" href="logbook.html#182" class="Datatype">ℕ</a> <a id="14400" class="Symbol">→</a> <a id="14402" href="logbook.html#250" class="Datatype">List</a> <a id="14407" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="14413" href="logbook.html#14387" class="Function">downFrom</a> <a id="14422" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a>    <a id="14430" class="Symbol">=</a> <a id="14432" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="14439" href="logbook.html#14387" class="Function">downFrom</a> <a id="14448" class="Symbol">(</a><a id="14449" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="14453" href="logbook.html#14453" class="Bound">n</a><a id="14454" class="Symbol">)</a> <a id="14456" class="Symbol">=</a> <a id="14458" href="logbook.html#14453" class="Bound">n</a> <a id="14460" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="14462" href="logbook.html#14387" class="Function">downFrom</a> <a id="14471" href="logbook.html#14453" class="Bound">n</a> 

    <a id="Example.mapDouble"></a><a id="14479" href="logbook.html#14479" class="Function">mapDouble</a> <a id="14489" class="Symbol">:</a> <a id="14491" href="logbook.html#250" class="Datatype">List</a> <a id="14496" href="logbook.html#182" class="Datatype">ℕ</a> <a id="14498" class="Symbol">→</a> <a id="14500" href="logbook.html#250" class="Datatype">List</a> <a id="14505" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="14511" href="logbook.html#14479" class="Function">mapDouble</a> <a id="14521" href="logbook.html#273" class="InductiveConstructor">[]</a>       <a id="14530" class="Symbol">=</a> <a id="14532" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="14539" href="logbook.html#14479" class="Function">mapDouble</a> <a id="14549" class="Symbol">(</a><a id="14550" href="logbook.html#14550" class="Bound">x</a> <a id="14552" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="14554" href="logbook.html#14554" class="Bound">xs</a><a id="14556" class="Symbol">)</a> <a id="14558" class="Symbol">=</a> <a id="14560" href="logbook.html#14550" class="Bound">x</a> <a id="14562" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">+</a> <a id="14564" href="logbook.html#14550" class="Bound">x</a> <a id="14566" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="14568" href="logbook.html#14479" class="Function">mapDouble</a> <a id="14578" href="logbook.html#14554" class="Bound">xs</a>

    <a id="Example.sum"></a><a id="14586" href="logbook.html#14586" class="Function">sum</a> <a id="14590" class="Symbol">:</a> <a id="14592" href="logbook.html#250" class="Datatype">List</a> <a id="14597" href="logbook.html#182" class="Datatype">ℕ</a> <a id="14599" class="Symbol">→</a> <a id="14601" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="14607" href="logbook.html#14586" class="Function">sum</a> <a id="14611" href="logbook.html#273" class="InductiveConstructor">[]</a>       <a id="14620" class="Symbol">=</a> <a id="14622" class="Number">0</a>
    <a id="14628" href="logbook.html#14586" class="Function">sum</a> <a id="14632" class="Symbol">(</a><a id="14633" href="logbook.html#14633" class="Bound">x</a> <a id="14635" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="14637" href="logbook.html#14637" class="Bound">xs</a><a id="14639" class="Symbol">)</a> <a id="14641" class="Symbol">=</a> <a id="14643" href="logbook.html#14633" class="Bound">x</a> <a id="14645" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">+</a> <a id="14647" href="logbook.html#14586" class="Function">sum</a> <a id="14651" href="logbook.html#14637" class="Bound">xs</a>

    <a id="Example.main"></a><a id="14659" href="logbook.html#14659" class="Function">main</a> <a id="14664" class="Symbol">=</a> <a id="14666" href="logbook.html#14586" class="Function">sum</a> <a id="14670" class="Symbol">(</a><a id="14671" href="logbook.html#14479" class="Function">mapDouble</a> <a id="14681" class="Symbol">(</a><a id="14682" href="logbook.html#14387" class="Function">downFrom</a> <a id="14691" class="Number">10000</a><a id="14696" class="Symbol">))</a> 
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

  <pre class="Agda">  <a id="24505" class="Keyword">module</a> <a id="DownFrom"></a><a id="24512" href="logbook.html#24512" class="Module">DownFrom</a> <a id="24521" class="Keyword">where</a>

    <a id="DownFrom.downFrom"></a><a id="24532" href="logbook.html#24532" class="Function">downFrom</a> <a id="24541" class="Symbol">:</a> <a id="24543" href="logbook.html#182" class="Datatype">ℕ</a> <a id="24545" class="Symbol">→</a> <a id="24547" href="logbook.html#250" class="Datatype">List</a> <a id="24552" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="24558" href="logbook.html#24532" class="Function">downFrom</a> <a id="24567" href="Agda.Builtin.Nat.html#221" class="InductiveConstructor">zero</a> <a id="24572" class="Symbol">=</a> <a id="24574" href="logbook.html#273" class="InductiveConstructor">[]</a>
    <a id="24581" href="logbook.html#24532" class="Function">downFrom</a> <a id="24590" class="Symbol">(</a><a id="24591" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="24595" href="logbook.html#24595" class="Bound">n</a><a id="24596" class="Symbol">)</a> <a id="24598" class="Symbol">=</a> <a id="24600" href="Agda.Builtin.Nat.html#234" class="InductiveConstructor">suc</a> <a id="24604" href="logbook.html#24595" class="Bound">n</a> <a id="24606" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="24608" href="logbook.html#24532" class="Function">downFrom</a> <a id="24617" href="logbook.html#24595" class="Bound">n</a> 

    <a id="DownFrom.sum"></a><a id="24625" href="logbook.html#24625" class="Function">sum</a> <a id="24629" class="Symbol">:</a> <a id="24631" href="logbook.html#250" class="Datatype">List</a> <a id="24636" href="logbook.html#182" class="Datatype">ℕ</a> <a id="24638" class="Symbol">→</a> <a id="24640" href="logbook.html#182" class="Datatype">ℕ</a>
    <a id="24646" href="logbook.html#24625" class="Function">sum</a> <a id="24650" href="logbook.html#273" class="InductiveConstructor">[]</a> <a id="24653" class="Symbol">=</a> <a id="24655" class="Number">0</a>
    <a id="24661" href="logbook.html#24625" class="Function">sum</a> <a id="24665" class="Symbol">(</a><a id="24666" href="logbook.html#24666" class="Bound">x</a> <a id="24668" href="logbook.html#290" class="InductiveConstructor Operator">∷</a> <a id="24670" href="logbook.html#24670" class="Bound">xs</a><a id="24672" class="Symbol">)</a> <a id="24674" class="Symbol">=</a> <a id="24676" href="logbook.html#24666" class="Bound">x</a> <a id="24678" href="Agda.Builtin.Nat.html#336" class="Primitive Operator">+</a> <a id="24680" href="logbook.html#24625" class="Function">sum</a> <a id="24684" href="logbook.html#24670" class="Bound">xs</a>

    <a id="DownFrom.main"></a><a id="24692" href="logbook.html#24692" class="Function">main</a> <a id="24697" class="Symbol">=</a> <a id="24699" href="logbook.html#24625" class="Function">sum</a> <a id="24703" class="Symbol">(</a><a id="24704" href="logbook.html#24532" class="Function">downFrom</a> <a id="24713" class="Number">4</a><a id="24714" class="Symbol">)</a>
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

