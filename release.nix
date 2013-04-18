/* Build instructions for the continuous integration system Hydra. */

{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, hledgerSrc ? { outPath = ../hledger; revCount = 0; gitTag = "dirty"; }
}:

let
  version = src.gitTag;
  versionSuffix = "";
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
in
rec {
  build = genAttrs [ "ghc742" "ghc762" ] (ghcVer: genAttrs [ "x86_64-linux" ] (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
    in
    pkgs.releaseTools.nixBuild {
      name = "hledger-interest";
      src = src;
      buildInputs = with haskellPackages; [
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
    }));
}
