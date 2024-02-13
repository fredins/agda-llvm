{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    Agda.url = "github:fredins/agda";
    Agda.flake = true;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          packages = {
            Agda.source = inputs.Agda;
          };

          settings = {
            Agda.check = false;
          };

          devShell = {
            enable = true;
            tools = hp: { 
              # Compling agda programs 
              zlib = pkgs.zlib;
              # LLVM with assertions (requires compilation)
              # libllvm = pkgs.llvmPackages_15.libllvm.override{debugVersion = true;};
              libllvm = pkgs.llvmPackages_15.libllvm;
              clangUseLLVM = pkgs.llvmPackages_15.clangUseLLVM;
              lld_15 = pkgs.lld_15;

              # Debugging
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
