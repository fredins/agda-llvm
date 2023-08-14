An Agda backend which compiles to LLVM via the intermidate representation GRIN [Boquist 1996].  

### Goal of the project

The goal is to compile Agda to GRIN [Boquist 1996], and to extend GRIN with `dup` and `drop` from Perceus's [Reinking et al. 2020] linear calculus. This will allow precise reference counting and memory reuse.  

### Dependencies

- ghc
- cabal-install
- zlib

### Build from source

The project depends on features from the unreleased Agda 2.6.4, which is why there is an agda submodule. If you already
have a local installation of Agda you may instead modify the `cabal.project` file, and skip the submodule.  

```
git clone --recurse-submodules https://github.com/fredins/agda-llvm.git
```

Running `make` will build and install the exectuable `agda-llvm` usually to `$HOME/.cabal/bin/`. This will build all dependencies and the agda submodule, which is over 400 modules. 

To run the compiler use the `--llvm` flag.  

```
agda-llvm --llvm agda-programs/DownFrom.agda
```

Many programs use the standard library which needs to be installed and configured separately, see [agda.readthedocs.io/en/latest/getting-started/installation.html](https://agda.readthedocs.io/en/latest/getting-started/installation.html) and [agda.readthedocs.io/en/latest/tools/package-system.html#use-std-lib](https://agda.readthedocs.io/en/latest/tools/package-system.html#use-std-lib).  

### Logbook
See [fredins.github.io](https://fredins.github.io)

### Paper (WIP)

See `latex/report.pdf`

### References
See `bibliography.bib`
