with import <nixpkgs> {};

let
drv = (haskellPackages.override {
  overrides = self: super: rec {
  };
}).callCabal2nix "m2h" ./. {};
in if lib.inNixShell then drv.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
}) else drv
