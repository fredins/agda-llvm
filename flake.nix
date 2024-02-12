{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    agda.url = "github:fredins/agda";
    agda.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          packages = {
            agda.source = inputs.agda;
          };
          devShell = {
            enable = true;
            tools = hp: { 
              # Compling agda programs 
              zlib = pkgs.zlib;
              # LLVM with assertions (require compilation)
              # libllvm = pkgs.llvmPackages_15.libllvm.override{debugVersion = true;};
              libllvm = pkgs.llvmPackages_15.libllvm;
              clangUseLLVM = pkgs.llvmPackages_15.clangUseLLVM;

              # Debugging
              lldb_15 = pkgs.lldb_15;
              valgrind = pkgs.valgrind;
              massif-visualizer = pkgs.massif-visualizer;

              # Call graphs
              graphviz = pkgs.graphviz;

              # Logbook HTML document
              pandoc = pkgs.pandoc;
            };
          };
        };

        packages.default = self'.packages.agda-llvm;
      };
    };
}
