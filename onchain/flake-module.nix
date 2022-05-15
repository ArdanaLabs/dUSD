{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit pkgs, plutusProjectIn
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self; };
      inherit (dusd-lib) pkgs plutusProjectIn;

      subdir = "onchain";
      project = plutusProjectIn {
        inherit subdir;
        extraShell = {
          additional = ps: [
            ps.apropos
            ps.apropos-tx
            ps.plutarch
            ps.plutarch-extra
            ps.sydtest
            ps.sydtest-hedgehog
          ];
        };
        pkg-def-extras = [ ];
        sha256map = import ./sha256map.nix;
      };
      haskellNixFlake = project.flake { };
    in
    {
      packages = haskellNixFlake.packages // {
        onchain-scripts = pkgs.stdenv.mkDerivation {
          name = "onchain-scripts";
          src = self; # FIXME: Why should src be project root here?
          buildInputs = [ haskellNixFlake.packages."dUSD-onchain:exe:scripts" ];
          doCheck = false;
          installPhase = ''
            scripts "$out"
          '';
          configurePhase = ''
            mkdir $out
          '';
        };
      };
      checks = haskellNixFlake.checks // { };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = {
  };
}