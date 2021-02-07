{
  description = "Space Invader Game for Kayla";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    nixpkgs.url = "github:nixos/nixpkgs/master";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      with (import nixpkgs { inherit system; });
      with haskellPackages;
      with haskell.lib;
      rec {
        packages = flattenTree {
          svaders = overrideCabal (callCabal2nix "svaders" ./. {}) (_: {
            librarySystemDepends = [ libGL libGLU ];
            doHaddock = false;
            doCheck = false;
          });
        };
        defaultPackage = packages.svaders;
      });
}
