/* Build instructions for the continuous integration system Hydra. */

{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, hledgerSrc ? { outPath = ../hledger; revCount = 0; gitTag = "dirty"; }
, officialRelease ? false
}:

let
  pkgs = import <nixpkgs> { };
  version = src.gitTag;
  versionSuffix = "";
in
rec {
  build = pkgs.lib.genAttrs [ "x86_64-linux" ] (system:
    let pkgs = import <nixpkgs> { inherit system; }; in
    pkgs.releaseTools.nixBuild {
      name = "hledger-interest";
      src = src;
      buildInputs = with pkgs.haskellPackages_ghc762; [
        ghc cabalDev cabalInstall
        cmdargs csv mtl parsec prettyShow regexCompat regexpr safe split
        time transformers utf8String HUnit
      ];
      configurePhase = ''
        cp -r ${hledgerSrc}/hledger-lib/ hledger-lib/
        chmod -R u+rw hledger-lib/
        cabal-dev add-source hledger-lib/
        cabal-dev install-deps
        cabal-dev configure -v
      '';
      buildPhase = "cabal-dev build";
      installPhase = "cabal-dev install";
      checkPhase = "cabal-dev test";
    });
}
