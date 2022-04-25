{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        t = lib.trivial;
        hl = haskell.lib;

        name = "names-and-faces";

        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in haskell.packages.ghc902.developPackage {
            root = (lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" "LICENSE" ]);
            name = name;
            returnShellEnv = !(devTools == [ ]);

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in {
        packages.pkg = project [ ];

        defaultPackage = self.packages.${system}.pkg;

        devShell = project (with haskell.packages.ghc902; [
          cabal-install
          haskell-language-server
          hlint
        ]);
      });
}
