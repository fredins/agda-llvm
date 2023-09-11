with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "agda-llvm"; 
    buildInputs = 
    [ 
      # Required for linking the compiler
      zlib 

      # Required for compling agda programs 
      (llvmPackages_15.libllvm.override{debugVersion = true;})
      llvmPackages_15.clangUseLLVM

      # Debugging
      lldb_15 
      valgrind

      # Required for the logbook HTML document
      pandoc
      ];
}
