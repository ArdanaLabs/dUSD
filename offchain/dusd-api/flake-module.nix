{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) dusd-lib offchain-lib;

      dusd-api = {
        dependencies =
          with ps-pkgs;
          [
            ctl-pkgs.aeson
            aff
            bigints
            ctl-pkgs.cardano-transaction-lib
            node-fs-aff
            ordered-collections
            aff-retry
            self'.packages."offchain:dusd-cbor"
          ];
        test-dependencies =
          with ps-pkgs;
          [
            node-process
            spec
          ];
        ps =
          purs-nix.purs
            {
              inherit (dusd-api) dependencies test-dependencies;
              dir = ./.;
            };
        package =
          purs-nix.build
            {
              name = "dusd-api";
              src.path = ./.;
              info = {
                inherit (dusd-api) dependencies;
                version = "0.0.1";
              };
            };
      };

      dusd-api-tests = { mode, runVolumeTests ? false }:
        pkgs.writeShellApplication
          {
            name = "dusd-api-tests";
            runtimeInputs = [
              pkgs.nodejs
              inputs'.yubihsm.packages.default
            ] ++ pkgs.lib.optionals (mode == "local") [
              pkgs.postgresql
              self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
              self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
              self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
              self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
            ];
            text =
              pkgs.lib.optionalString runVolumeTests "export RUN_VOLUME_TESTS=1" + ''
                export MODE=${mode}
                export TEST_RESOURCES=${./fixtures}
                export NODE_PATH=${config.ctl.nodeModules}/node_modules
                ${dusd-api.ps.test.run { }}
              '';
          };
    in
    {
      apps = {
        "offchain:dusd-api:test:testnet" = dusd-lib.mkApp (dusd-api-tests { mode = "testnet"; });
        "offchain:dusd-api:test:local" = dusd-lib.mkApp (dusd-api-tests { mode = "local"; });
      };
      checks = {
        run-dusd-api-tests =
          let test = dusd-api-tests { mode = "local"; }; in
          pkgs.runCommand test.name { }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
        run-dusd-api-volume-tests =
          let test = dusd-api-tests { mode = "local"; runVolumeTests = true; }; in
          pkgs.runCommand test.name { }
            "${test}/bin/${test.meta.mainProgram} | tee $out";
      };
      devShells."offchain:dusd-api" =
        offchain-lib.makeProjectShell { project = dusd-api; extraBuildInputs = [ inputs'.yubihsm.packages.default ]; };
      packages = {
        "offchain:dusd-api" = dusd-api.package;
        "offchain:dusd-api:docs" =
          pkgs.runCommand "dusd-api-docs" { }
            ''
              mkdir $out && cd $out
              # it may make sense to eventually add cli and browser to the srcs, but we need to not define Main twice
              ${dusd-api.ps.command { srcs = [ ./src ];}}/bin/purs-nix docs
            '';
      };
    };
  flake = { };
}
