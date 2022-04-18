{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc901", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clay, lib, lucid, text, cabal-install, haskell-language-server }:
      mkDerivation {
        pname = "names-and-faces";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base clay lucid text cabal-install haskell-language-server ];
        license = lib.licenses.agpl3Plus;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
