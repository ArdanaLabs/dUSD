{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
  let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit plutusProjectIn
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self; };
      inherit (dusd-lib) plutusProjectIn;

      subdir = "onchain";
      project = plutusProjectIn {
        inherit subdir;
        inherit pkgs;
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
        sha256map = import ./sha256map;
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
      checks = haskellNixFlake.checks // {
        end-to-end = self.inputs.maesarat.lib.${system}.end-to-end {
          cardano-node = inputs'.cardano-node.packages.cardano-node;
          cardano-cli = inputs'.cardano-node.packages.cardano-cli;
          plutus-apps = self.inputs.plutus-apps;
          e2eBinary = self'.packages."hello-world:exe:hello-world-e2e";
        };
      };
      devShells.onchain = haskellNixFlake.devShell // { };
    };
  flake = {
  };
}
