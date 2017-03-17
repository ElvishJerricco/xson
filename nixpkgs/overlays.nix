nixself: nixsuper: {
  haskellPackages = nixsuper.haskellPackages.extend (self: super:
    let
      lib = nixself.haskell.lib;
      # We always want benchmark depends.
      callWithBench = name: src: args:
        lib.overrideCabal (self.callCabal2nix name src args) (drv: {
          withBenchmarkDepends = true;
        });
    in {
      # Override callCabal2nix to make it cache cabal files
      # better. Only temporary until this pull request hits the
      # nixpkgs-unstable channel
      #
      # https://github.com/NixOS/nixpkgs/pull/23880
      callCabal2nix = name: src: args:
        let
          # Filter out files other than the cabal file. This ensures
          # that we don't create new derivations even when the cabal
          # file hasn't changed.
          justCabal = builtins.filterSource (path: type: nixself.lib.hasSuffix ".cabal" path) src;
          drv = super.callCabal2nix name justCabal args;
        in lib.overrideCabal drv (drv': { inherit src; }); # Restore the desired src.

      # Local packages
      xson = callWithBench "xson" ../xson {};
      xson-pipes = callWithBench "xson-pipes" ../xson-pipes {};
      xson-conduit = callWithBench "xson-conduit" ../xson-conduit {};
      xson-bench-test = lib.dontHaddock (callWithBench "xson-bench-test" ../xson-bench-test {});
    });
}
