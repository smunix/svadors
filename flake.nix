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
      let fixConfigFlags = drv: appendConfigureFlags drv [
            "--ghc-option=-optl=-L${libGL}/lib"
            "--ghc-option=-optl=-L${libGLU}/lib"
            # "--ghc-option=-optl=-L${mesa}/lib"
            "--ghc-options=-optl=-L${freeglut}/lib"
            "--ghc-options=-optl=-lglut"
            "--ghc-options=-optl=-lGL"
            "--ghc-options=-optl=-lGLU"
          ];
          OpenGLRaw = fixConfigFlags (overrideCabal (callHackage "OpenGLRaw" "3.3.4.0" {}) (_: {
            librarySystemDepends = [ libGL libGLU ];
          }));
          GLURaw = fixConfigFlags (overrideCabal (callHackage "GLURaw" "2.0.0.4" {
            inherit
              OpenGLRaw
            ;
          }) (_: {
            librarySystemDepends = [ libGL libGLU ];
          }));
          OpenGL = fixConfigFlags (overrideCabal (callHackage "OpenGL" "3.0.3.0" {
            inherit
              OpenGLRaw
              GLURaw
            ;
          }) (_: {
            librarySystemDepends = [ libGL libGLU ];
          }));
          gloss = fixConfigFlags (overrideCabal (callHackage "gloss" "1.13.2.1" {
            inherit
              OpenGL
            ;
          }) (_: {
            librarySystemDepends = [ libGL libGLU ];
          }));
      in {
        packages = flattenTree rec {
          svaders =
            fixConfigFlags (overrideCabal (callCabal2nix "svaders" ./. {
              # inherit
              #   gloss
              # ;
            }) (_: {
              librarySystemDepends = [ libGL libGLU ];
              doHaddock = false;
            }));
        };
      });
}
