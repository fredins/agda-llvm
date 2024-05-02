An Agda backend that compiles to LLVM via the intermidate representation GRIN (Boquist 1999, Johnsson 1991).
Memory is managed by Perceus-style (Reiking et al. 2021) reference counting.

- [Installation](INSTALL.md) 
- [Documentation regarding code structure and contributions](CONTRIBUTING.md)
- [Work-in-progress paper](latex/report.pdf)
- [References](bibliography.bib)

### Restrictions
- No lambdas.  
- No higher-order functions or partial applications.  
- No parametric polymorphism.  
- No records
- Only pure functions.

### Known issues
- Super ugly code.  
- Heap points-to analysis doesn't terminate for all programs.  
- All functions in scope must be reachable from `main`. This 
  includes imports, and `using`/`hiding` have no effect.  
- Only a small number of primitives are supported (`NATURAL`, `NATPLUS`, `NATMINUS`).
- Integer overflows.
- There is no strictness/demand analysis and all functions calls are lazy. Therefore,  
  space leaks are common and evaluating a huge thunk blows up the stack. This can be 
  manually mitigated by using `primForce`.
- Proofs are not erased properly.

