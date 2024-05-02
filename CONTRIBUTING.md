
### Contributing
Feel free to open an issue if you find an error, however, don't expected it to be solved anytime in the near future. I will concentrate on the problems outlined in section [TODO](#todo). Pull requests are welcomed but reviewing is kind of hard as we do not have any tests. Instead, the best way to get involved is to reach out to me via email (and please do!).

### Code structure

- [app/Main.hs](app/Main.hs) is the entry point.
- [src/Compiler](src/Compiler) contains the code for the compiler split into the three syntaxes [Treeless](src/Compiler/Treeless), [GRIN](src/Compiler/Grin), and [LLVM](src/Compiler/Llvm). Notably, [Llvm/Compiler.hs](src/Compiler/Llvm/Compiler.hs) defines the Agda backend, and it is entry point right after [app/Main.hs](app/Main.hs).
- [src/Formalize](src/Formalize) is the start of a formalization of the Perceus algorithm. We use `agda2hs` to generate readable Haskell code, which is placed in the [gen](gen) directory. See [src/Formalize/README.md](src/Formalize/README.md) for more information.

### TODO
1. Formalize Perceus algorithm
2. Add the big data node layout
3. Switch over to the new heap points-to analysis which 
   should fix all termination issues.
4. Add higher-order functions
5. Implement the rest of the GRIN optimizations
6. Implement the Perceus reuse-analysis
