
This is an attempt to formalize our variant of the Perceus algorithm (src/Compiler/Grin/Perceus.hs). See, latex/report.pdf figure 3 for the current rules. We have modelled some of the rules in Formalize.MiniGrin. However, the rules are not identical as we use different datatypes and syntax representation that are easier to reason about. 

We have two dependencies, agda2hs and scope, that need to be installed manually. These are available at [github.com/agda/agda2hs](https://github.com/agda/agda2hs) and [github.com/jespercockx/scope](https://github.com/jespercockx/scope), respectively. We take inspiration from [github.com/jespercockx/agda-core](https://github.com/jespercockx/agda-core).

We eventually plan to replace the current Haskell implementation (src/Compiler/Grin/Perceus.hs) by using agda2hs. So, we are limited to features that agda2hs supports.

