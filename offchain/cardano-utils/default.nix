{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) dusd-lib offchain-lib;

      cardano-utils = {
        dependencies =
          with ps-pkgs;
          [
            aff-retry
            effect
            node-child-process
            node-path
            prelude
          ];
        test-dependencies =
          with ps-pkgs;
          [
            aff
            spec
            typelevel # for undefined
            self'.packages."offchain:cardano-utils"
            ctl-pkgs.cardano-transaction-lib # needed for withPlutipContractEnv
          ];
        ps =
          purs-nix.purs
            {
              inherit (cardano-utils) dependencies test-dependencies;
              dir = ./.;
            };
        package =
          purs-nix.build
            {
              name = "cardano-utils";
              src.path = ./.;
              info = {
                inherit (cardano-utils) dependencies;
                version = "0.0.1";
              };
            };
      };
    in
    {
      devShells."offchain:cardano-utils" =
        offchain-lib.makeProjectShell cardano-utils { };
      packages = {
        "offchain:cardano-utils" = cardano-utils.package;
        "offchain:cardano-utils:docs" =
          pkgs.runCommand "cardano-utils-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${cardano-utils.ps.command { srcs = [ ./src ];}}/bin/purs-nix docs
            '';
      };
      checks."offchain:cardano-utils:test" = cardano-utils.ps.test.check { };
    };
  flake = { };
}
