
This is an attempt to formalize our variant of the Perceus algorithm [src/Compiler/Grin/Perceus.hs](../Compiler/Grin/Perceus.hs). See, [report.pdf](../../latex/report.pdf) Figure 3 for the current rules. We have modelled some of the rules in [MiniGrin.agda](MiniGrin.agda). However, the rules are not identical as we use different datatypes and syntax representation that are easier to reason about. 

Our only dependency is [agda2hs](https://github.com/agda/agda2hs). We take inspiration from [github.com/jespercockx/agda-core](https://github.com/jespercockx/agda-core). Our syntax representation uses co-de-Bruijn indices [(McBride, 2018)](https://arxiv.org/abs/1807.04085) but the implementation is similar to [github.com/jespercockx/scope](https://github.com/jespercockx/scope).

We eventually plan to replace the current Haskell implementation [src/Compiler/Grin/Perceus.hs](../Compiler/Grin/Perceus.hs) by using `agda2hs`. So, we are limited to features that `agda2hs` supports.

### Getting started

Start by taking a look at [Test.agda](Test.agda) for a short introduction.

`make gen` will generate the corresponding Haskell code to the [gen](../../gen) directory.

You can also run `cabal repl gen` to interpret the generated code.

### Status

We have rules and implementation for the term constructors `Return`, `AppDef`, `Bind`, and the only values we support are variables `Var`. These constructors involve two challenging aspects: composition of terms and variable bindings. So, other constructors such as `Store` and `Fetch` are straight forward to implement. `Case`, `Update`, and literals `Lit` will, however, require a bit more work. Our current goal is to hold off on implementing all the rules, and instead focus on proving properties about the rules. Following are some of the properties we are intrested in.

- Reachability. All heap nodes are reachable from the pointers at the stack. 
  For any heap H : Loc → HeapNode and stack S : Abs → Val, the domain dom(H) ∈ image(S).

- Inverse Reachability. All pointers points to a location that is part of the heap. 
  Thereby, we have no "use after free".
  
- No garbage. Given reachability, we can prove that there is no garbage left behind, i.e. 
  all the memory is deallocated when the program finishes (empty heap). 
  TODO: make local, doesn't touch the initial heap.

- Number of reference counts equal to number of actual references. 
  TODO: define "actual references"

- Precision. Nodes are deallocated as soon as the node is no longer need. 
  (Reiking et al. proves this by proving soundness from the syntax-directed 
  linear rules to declaraitve linear syntax rules.)

Another future goal is to connect the generated code to the compiler pipeline. 
