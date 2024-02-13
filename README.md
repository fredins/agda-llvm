An Agda backend that compiles to LLVM via the intermidate representation GRIN (Boquist 1999, Johnsson 1991).
Memory is managed by Perceus-style (Reiking et al. 2021) reference counting.

### Dependencies

- ghc
- cabal-install
- zlib
- clang
- lld (LLVM linker)

### Build from source

The easiest way to satisfy all the dependencies is by using the nix flake: 

```
nix --experimental-features 'nix-command flakes' develop
```

Running `make install` will build and install the exectuable `agda-llvm` usually to `$HOME/.cabal/bin/` or `$HOME/.local/bin`. 

To run the compiler use the `--llvm` flag.  

```
agda-llvm --llvm <FILE>
```

To run the GRIN interpreter add the `--interpret` flag.

```
agda-llvm --llvm --interpret <FILE>
```

There is also an optimization flag `-O` that enable O3 optimizations and LTO in clang.

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

### TODO
1. Formalize Perceus algorithm
2. Add the big data node layout
3. Switch over to the new heap points-to analysis which 
   should fix all termination issues.
4. Add higher-order functions
5. Implement the rest of the GRIN optimizations
6. Implement the Perceus reuse-analysis

### Logbook (OUT OF DATE)
See [fredins.github.io](https://fredins.github.io)

### Paper (WIP)

See `latex/report.pdf`

### References
See `bibliography.bib`
