{ nixpkgs ? import ./nixpkgs {}
, ghc ? null # Stack gives us this. We don't want it.
}:

nixpkgs.haskell.lib.overrideCabal (nixpkgs.haskellPackages.callCabal2nix "xson" ./. {}) (drv: {
  withBenchmarkDepends = true;
})
