{ compiler ? "ghc843", doBenchmark ? false }:

let
  bootstrap = import <nixpkgs> { };
  nixpkgsJson = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  nixpkgsSrc = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgsJson) rev sha256;
  };
  nixpkgs = import nixpkgsSrc {};

  giGtkDeclarativeJson = builtins.fromJSON (builtins.readFile ./gi-gtk-declarative.json);

  inherit (nixpkgs) pkgs;

  giGtkDeclarative = nixpkgs.fetchFromGitHub {
    owner = "owickstrom";
    repo = "gi-gtk-declarative";
    inherit (giGtkDeclarativeJson) rev sha256;
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      gi-gtk-declarative = self.callCabal2nix "gi-gtk-declarative" "${giGtkDeclarative}/gi-gtk-declarative" {};
      gi-gtk-declarative-app-simple = self.callCabal2nix "gi-gtk-declarative-app-simple" "${giGtkDeclarative}/gi-gtk-declarative-app-simple" {};
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "declarative-gtk-examples" ./. {});
in
{ declarative-gtk-examples = drv;
  declarative-gtk-examples-shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [drv];
  };
}
