
### Dependencies

- ghc
- cabal-install
- zlib
- clang
- lld (LLVM linker)
- valgrind (optional)

### Build from source

The easiest way to satisfy all the dependencies is by using the nix flake: 

```
nix --experimental-features 'nix-command flakes' develop
```

Running `make` will build and install the exectuable `agda-llvm` usually to `$HOME/.cabal/bin/` or `$HOME/.local/bin`. 

To run the compiler use the `--llvm` flag.  

```
agda-llvm --llvm <FILE>
```

To run the GRIN interpreter add the `--interpret` flag.

```
agda-llvm --llvm --interpret <FILE>
```

There is also an optimization flag `-O` that enable O3 optimizations and LTO in clang.

Optionally, you can run `./simple-test` to compile and run a few selected programs. This step requires valgrind.
