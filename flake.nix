{
  description = "Space Invader Game for Kayla";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    nixpkgs.url = "github:nixos/nixpkgs/master";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem ["x86_64-linux"] (system:
      with (import nixpkgs { inherit system; });
      with haskellPackages;
      with haskell.lib;
      rec {
        packages = flattenTree {
          svaders =
            fixConfigFlags (overrideCabal (callCabal2nix "svaders" ./. {}) (_: {
              librarySystemDepends = [ libGL libGLU ];
              doHaddock = false;
            }));
        };
        defaultPackage = packages.svaders;
      });
}
