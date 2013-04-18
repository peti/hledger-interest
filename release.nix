/* Build instructions for the continuous integration system Hydra. */

{ hledgerSrc ? { outPath = ../hledger; revCount = 0; gitTag = "dirty"; }
, hledgerInterestSrc ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
}:

let
  genAttrs = (import <nixpkgs> { }).lib.genAttrs;
  supportedCompilers = [ "ghc704" "ghc742" "ghc762" ];
  supportedPlatforms = [ "x86_64-linux" ];
in
rec {
  hledgerLib = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "hledger-lib";
      src = hledgerSrc;
      version = hledgerSrc.gitTag;
      postUnpack = "sourceRoot+=/hledger-lib";
      buildDepends = with haskellPackages; [
        cmdargs csv filepath HUnit mtl parsec prettyShow regexCompat
        regexpr safe split time transformers utf8String
      ];
    })));

  hledger = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
      myHledgerLib = pkgs.lib.getAttrFromPath [ghcVer system] hledgerLib;
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "hledger";
      src = hledgerSrc;
      version = hledgerSrc.gitTag;
      postUnpack = "sourceRoot+=/hledger";
      buildDepends = with haskellPackages; [ myHledgerLib haskeline shakespeareText ];
    })));

  hledgerWeb = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
      myHledgerLib = pkgs.lib.getAttrFromPath [ghcVer system] hledgerLib;
      myHledger = pkgs.lib.getAttrFromPath [ghcVer system] hledger;
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "hledger-web";
      src = hledgerSrc;
      version = hledgerSrc.gitTag;
      postUnpack = "sourceRoot+=/hledger-web";
      buildDepends = with haskellPackages; [
        blazeHtml blazeMarkup clientsession cmdargs dataDefault filepath
        hamlet hjsmin myHledger myHledgerLib httpConduit HUnit monadControl
        networkConduit parsec regexpr safe shakespeareCss shakespeareJs
        shakespeareText text time transformers wai waiExtra warp yaml yesod
        yesodCore yesodDefault yesodForm yesodStatic blazeHtml
      ];
      testDepends = with haskellPackages; [ yesodCore yesodDefault yesodTest ];
    })));

  hledgerInterest = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
      myHledgerLib = pkgs.lib.getAttrFromPath [ghcVer system] hledgerLib;
    in
    haskellPackages.cabal.mkDerivation (self: {
      pname = "hledger-interest";
      src = hledgerInterestSrc;
      version = hledgerInterestSrc.gitTag;
      buildDepends = with haskellPackages; [ myHledgerLib mtl ];
    })));
}
