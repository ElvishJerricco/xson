let overlays = import ./overlays.nix;
in { ... } @ args:

import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs-channels";
  rev = "2839b101f927be5daab7948421de00a6f6c084ae";
  sha256 = "0a863cc5462gn1vws87d4qn45zk22m64ri1ip67w0b1a9bmymqdh";
}) (args // {
  overlays = [overlays] ++ (args.overlays or []);
})
