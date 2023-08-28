with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "agda-llvm"; 
    buildInputs = [ zlib ];
}
