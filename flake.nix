{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , nixpkgs
    , flake-compat
    , flake-compat-ci
    , flake-utils
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
    let
        pkgs = import nixpkgs {inherit system; };
        latexEnv = [ (pkgs.texlive.combine { inherit ( pkgs.texlive ) scheme-basic latexmk; })];
    in
    {

      devShell =
        pkgs.mkShell
        {
          # with pkgs;
          name = "dUSD";
          buildInputs = [ pkgs.entr latexEnv];
        };
      apps =
        {
          feedback-loop = {
            type = "app";
            program = "${ pkgs.callPackage ./documentation/test-plan-loop.nix { } }/bin/feedback-loop";
          };
        };
      ciNix =
        flake-compat-ci.lib.recurseIntoFlakeWith
          {
            flake = self;
            systems = [ "x86_64-linux" ];
          };
    });
}
