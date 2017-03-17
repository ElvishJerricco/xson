{ nixpkgs ? import ../nixpkgs {}
, ghc ? null
}:

nixpkgs.haskellPackages.xson-conduit
