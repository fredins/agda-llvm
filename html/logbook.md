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

  <pre class="Agda">
  <a id="1680" class="Keyword">open</a> <a id="1685" class="Keyword">import</a> <a id="1692" href="Agda.Builtin.Nat.html" class="Module">Agda.Builtin.Nat</a> <a id="1709" class="Keyword">using</a> <a id="1715" class="Symbol">(</a><a id="1716" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a><a id="1719" class="Symbol">;</a> <a id="1721" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a><a id="1725" class="Symbol">;</a> <a id="1727" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">_+_</a><a id="1730" class="Symbol">)</a> <a id="1732" class="Keyword">renaming</a> <a id="1741" class="Symbol">(</a><a id="1742" href="Agda.Builtin.Nat.html#186" class="Datatype">Nat</a> <a id="1746" class="Symbol">to</a> <a id="1749" class="Datatype">ℕ</a><a id="1750" class="Symbol">)</a> 

  <a id="1756" class="Keyword">infixr</a> <a id="1763" class="Number">5</a> <a id="1765" href="logbook.html#1832" class="InductiveConstructor Operator">_∷_</a>
  <a id="1771" class="Keyword">data</a> <a id="List"></a><a id="1776" href="logbook.html#1776" class="Datatype">List</a> <a id="1781" class="Symbol">{</a><a id="1782" href="logbook.html#1782" class="Bound">a</a><a id="1783" class="Symbol">}</a> <a id="1785" class="Symbol">(</a><a id="1786" href="logbook.html#1786" class="Bound">A</a> <a id="1788" class="Symbol">:</a> <a id="1790" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="1794" href="logbook.html#1782" class="Bound">a</a><a id="1795" class="Symbol">)</a> <a id="1797" class="Symbol">:</a> <a id="1799" href="Agda.Primitive.html#320" class="Primitive">Set</a> <a id="1803" href="logbook.html#1782" class="Bound">a</a> <a id="1805" class="Keyword">where</a>
    <a id="List.[]"></a><a id="1815" href="logbook.html#1815" class="InductiveConstructor">[]</a>  <a id="1819" class="Symbol">:</a> <a id="1821" href="logbook.html#1776" class="Datatype">List</a> <a id="1826" href="logbook.html#1786" class="Bound">A</a>
    <a id="List._∷_"></a><a id="1832" href="logbook.html#1832" class="InductiveConstructor Operator">_∷_</a> <a id="1836" class="Symbol">:</a> <a id="1838" class="Symbol">(</a><a id="1839" href="logbook.html#1839" class="Bound">x</a> <a id="1841" class="Symbol">:</a> <a id="1843" href="logbook.html#1786" class="Bound">A</a><a id="1844" class="Symbol">)</a> <a id="1846" class="Symbol">(</a><a id="1847" href="logbook.html#1847" class="Bound">xs</a> <a id="1850" class="Symbol">:</a> <a id="1852" href="logbook.html#1776" class="Datatype">List</a> <a id="1857" href="logbook.html#1786" class="Bound">A</a><a id="1858" class="Symbol">)</a> <a id="1860" class="Symbol">→</a> <a id="1862" href="logbook.html#1776" class="Datatype">List</a> <a id="1867" href="logbook.html#1786" class="Bound">A</a>

  <a id="downFrom"></a><a id="1872" href="logbook.html#1872" class="Function">downFrom</a> <a id="1881" class="Symbol">:</a> <a id="1883" href="logbook.html#1749" class="Datatype">ℕ</a> <a id="1885" class="Symbol">→</a> <a id="1887" href="logbook.html#1776" class="Datatype">List</a> <a id="1892" href="logbook.html#1749" class="Datatype">ℕ</a>
  <a id="1896" href="logbook.html#1872" class="Function">downFrom</a> <a id="1905" href="Agda.Builtin.Nat.html#204" class="InductiveConstructor">zero</a> <a id="1910" class="Symbol">=</a> <a id="1912" href="logbook.html#1815" class="InductiveConstructor">[]</a>
  <a id="1917" href="logbook.html#1872" class="Function">downFrom</a> <a id="1926" class="Symbol">(</a><a id="1927" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="1931" href="logbook.html#1931" class="Bound">n</a><a id="1932" class="Symbol">)</a> <a id="1934" class="Symbol">=</a> <a id="1936" href="Agda.Builtin.Nat.html#217" class="InductiveConstructor">suc</a> <a id="1940" href="logbook.html#1931" class="Bound">n</a> <a id="1942" href="logbook.html#1832" class="InductiveConstructor Operator">∷</a> <a id="1944" href="logbook.html#1872" class="Function">downFrom</a> <a id="1953" href="logbook.html#1931" class="Bound">n</a> 
  
  <a id="sum"></a><a id="1961" href="logbook.html#1961" class="Function">sum</a> <a id="1965" class="Symbol">:</a> <a id="1967" href="logbook.html#1776" class="Datatype">List</a> <a id="1972" href="logbook.html#1749" class="Datatype">ℕ</a> <a id="1974" class="Symbol">→</a> <a id="1976" href="logbook.html#1749" class="Datatype">ℕ</a>
  <a id="1980" href="logbook.html#1961" class="Function">sum</a> <a id="1984" href="logbook.html#1815" class="InductiveConstructor">[]</a> <a id="1987" class="Symbol">=</a> <a id="1989" class="Number">0</a>
  <a id="1993" href="logbook.html#1961" class="Function">sum</a> <a id="1997" class="Symbol">(</a><a id="1998" href="logbook.html#1998" class="Bound">x</a> <a id="2000" href="logbook.html#1832" class="InductiveConstructor Operator">∷</a> <a id="2002" href="logbook.html#2002" class="Bound">xs</a><a id="2004" class="Symbol">)</a> <a id="2006" class="Symbol">=</a> <a id="2008" href="logbook.html#1998" class="Bound">x</a> <a id="2010" href="Agda.Builtin.Nat.html#319" class="Primitive Operator">+</a> <a id="2012" href="logbook.html#1961" class="Function">sum</a> <a id="2016" href="logbook.html#2002" class="Bound">xs</a>
  
  <a id="main"></a><a id="2024" href="logbook.html#2024" class="Function">main</a> <a id="2029" class="Symbol">=</a> <a id="2031" href="logbook.html#1961" class="Function">sum</a> <a id="2035" class="Symbol">(</a><a id="2036" href="logbook.html#1872" class="Function">downFrom</a> <a id="2045" class="Number">4</a><a id="2046" class="Symbol">)</a>
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

