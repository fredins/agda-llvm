# { pkgs ? import <nixpkgs> { } }:
# with pkgs;
# mkShell {
#   buildInputs = [
#     hlint
#     haskell-language-server
#     haskellPackages.cabal-install
#     haskellPackages.stylish-haskell
#   ];
# }

with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "testing";
  buildInputs = [ 
    zlib 
    graphviz
  ];
}
