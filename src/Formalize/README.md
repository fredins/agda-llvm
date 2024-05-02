
This is an attempt to formalize our variant of the Perceus algorithm [src/Compiler/Grin/Perceus.hs](../Compiler/Grin/Perceus.hs). See, [report.pdf](../../latex/report.pdf) Figure 3 for the current rules. We have modelled some of the rules in [MiniGrin.agda](MiniGrin.agda). However, the rules are not identical as we use different datatypes and syntax representation that are easier to reason about. 

Our only dependency is [agda2hs](https://github.com/agda/agda2hs). We take inspiration from  [github.com/jespercockx/agda-core](https://github.com/jespercockx/agda-core). Our syntax representation uses co-de-Bruijn indices [(McBride, 2018)](https://arxiv.org/abs/1807.04085) but the implementation is similar to [github.com/jespercockx/scope](https://github.com/jespercockx/scope).

We eventually plan to replace the current Haskell implementation [src/Compiler/Grin/Perceus.hs](../Compiler/Grin/Perceus.hs) by using `agda2hs`. So, we are limited to features that `agda2hs` supports.

### Getting started

Start by taking a look at [Test.agda](Test.agda) for a short introduction.

`make gen` will generate the corresponding Haskell code to the [gen](../../gen) directory.

You can also run `cabal repl gen` to interpret the generated code.
