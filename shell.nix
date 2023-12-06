with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "agda-llvm"; 
    buildInputs = 
    [ 
      # Required for compling agda programs 
      zlib 
      (llvmPackages_15.libllvm.override{debugVersion = true;})
      llvmPackages_15.clangUseLLVM

      # Debugging
      lldb_15 
      valgrind
      massif-visualizer

      # Required for the logbook HTML document
      pandoc

      # call graphs
      graphviz
      ];
}
