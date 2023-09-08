with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "agda-llvm"; 
    buildInputs = 
    [ zlib 
      (llvmPackages_15.libllvm.override{debugVersion = true;})
      llvmPackages_15.clangUseLLVM
      lldb_15 ];
}
