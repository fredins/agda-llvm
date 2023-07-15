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
- more... 

Did the following:

- Tried to implement STG-like thunks manually in LLVM IR.  
- Manually translated the the following program first to GRIN and then
  LLVM IR.  

  <pre class="Agda">
  <a id="1661" class="Keyword">open</a> <a id="1666" class="Keyword">import</a> <a id="1673" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="1690" class="Keyword">using</a> <a id="1696" class="Symbol">(</a><a id="1697" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="1700" class="Symbol">;</a> <a id="1702" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="1706" class="Symbol">;</a> <a id="1708" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="1711" class="Symbol">)</a> <a id="1713" class="Keyword">renaming</a> <a id="1722" class="Symbol">(</a><a id="1723" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="1727" class="Symbol">to</a> <a id="1730" class="Datatype">ℕ</a><a id="1731" class="Symbol">)</a> 

  <a id="1737" class="Keyword">infixr</a> <a id="1744" class="Number">5</a> <a id="1746" href="logbook.html#1813" class="InductiveConstructor Operator">_∷_</a>
  <a id="1752" class="Keyword">data</a> <a id="List"></a><a id="1757" href="logbook.html#1757" class="Datatype">List</a> <a id="1762" class="Symbol">{</a><a id="1763" href="logbook.html#1763" class="Bound">a</a><a id="1764" class="Symbol">}</a> <a id="1766" class="Symbol">(</a><a id="1767" href="logbook.html#1767" class="Bound">A</a> <a id="1769" class="Symbol">:</a> <a id="1771" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="1775" href="logbook.html#1763" class="Bound">a</a><a id="1776" class="Symbol">)</a> <a id="1778" class="Symbol">:</a> <a id="1780" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="1784" href="logbook.html#1763" class="Bound">a</a> <a id="1786" class="Keyword">where</a>
    <a id="List.[]"></a><a id="1796" href="logbook.html#1796" class="InductiveConstructor">[]</a>  <a id="1800" class="Symbol">:</a> <a id="1802" href="logbook.html#1757" class="Datatype">List</a> <a id="1807" href="logbook.html#1767" class="Bound">A</a>
    <a id="List._∷_"></a><a id="1813" href="logbook.html#1813" class="InductiveConstructor Operator">_∷_</a> <a id="1817" class="Symbol">:</a> <a id="1819" class="Symbol">(</a><a id="1820" href="logbook.html#1820" class="Bound">x</a> <a id="1822" class="Symbol">:</a> <a id="1824" href="logbook.html#1767" class="Bound">A</a><a id="1825" class="Symbol">)</a> <a id="1827" class="Symbol">(</a><a id="1828" href="logbook.html#1828" class="Bound">xs</a> <a id="1831" class="Symbol">:</a> <a id="1833" href="logbook.html#1757" class="Datatype">List</a> <a id="1838" href="logbook.html#1767" class="Bound">A</a><a id="1839" class="Symbol">)</a> <a id="1841" class="Symbol">→</a> <a id="1843" href="logbook.html#1757" class="Datatype">List</a> <a id="1848" href="logbook.html#1767" class="Bound">A</a>

  <a id="downFrom"></a><a id="1853" href="logbook.html#1853" class="Function">downFrom</a> <a id="1862" class="Symbol">:</a> <a id="1864" href="logbook.html#1730" class="Datatype">ℕ</a> <a id="1866" class="Symbol">→</a> <a id="1868" href="logbook.html#1757" class="Datatype">List</a> <a id="1873" href="logbook.html#1730" class="Datatype">ℕ</a>
  <a id="1877" href="logbook.html#1853" class="Function">downFrom</a> <a id="1886" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="1891" class="Symbol">=</a> <a id="1893" href="logbook.html#1796" class="InductiveConstructor">[]</a>
  <a id="1898" href="logbook.html#1853" class="Function">downFrom</a> <a id="1907" class="Symbol">(</a><a id="1908" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="1912" href="logbook.html#1912" class="Bound">n</a><a id="1913" class="Symbol">)</a> <a id="1915" class="Symbol">=</a> <a id="1917" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="1921" href="logbook.html#1912" class="Bound">n</a> <a id="1923" href="logbook.html#1813" class="InductiveConstructor Operator">∷</a> <a id="1925" href="logbook.html#1853" class="Function">downFrom</a> <a id="1934" href="logbook.html#1912" class="Bound">n</a> 
  
  <a id="sum"></a><a id="1942" href="logbook.html#1942" class="Function">sum</a> <a id="1946" class="Symbol">:</a> <a id="1948" href="logbook.html#1757" class="Datatype">List</a> <a id="1953" href="logbook.html#1730" class="Datatype">ℕ</a> <a id="1955" class="Symbol">→</a> <a id="1957" href="logbook.html#1730" class="Datatype">ℕ</a>
  <a id="1961" href="logbook.html#1942" class="Function">sum</a> <a id="1965" href="logbook.html#1796" class="InductiveConstructor">[]</a> <a id="1968" class="Symbol">=</a> <a id="1970" class="Number">0</a>
  <a id="1974" href="logbook.html#1942" class="Function">sum</a> <a id="1978" class="Symbol">(</a><a id="1979" href="logbook.html#1979" class="Bound">x</a> <a id="1981" href="logbook.html#1813" class="InductiveConstructor Operator">∷</a> <a id="1983" href="logbook.html#1983" class="Bound">xs</a><a id="1985" class="Symbol">)</a> <a id="1987" class="Symbol">=</a> <a id="1989" href="logbook.html#1979" class="Bound">x</a> <a id="1991" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="1993" href="logbook.html#1942" class="Function">sum</a> <a id="1997" href="logbook.html#1983" class="Bound">xs</a>
  
  <a id="main"></a><a id="2005" href="logbook.html#2005" class="Function">main</a> <a id="2010" class="Symbol">=</a> <a id="2012" href="logbook.html#1942" class="Function">sum</a> <a id="2016" class="Symbol">(</a><a id="2017" href="logbook.html#1853" class="Function">downFrom</a> <a id="2026" class="Number">4</a><a id="2027" class="Symbol">)</a>
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
- Started on the treeless-to-GRIN transformation, and the 
  GRIN-to-LLVM transformation.
- Checked out literate agda using both markdown and latex.  
- Prepared the documents for the logbook and the report.  

References
----------

