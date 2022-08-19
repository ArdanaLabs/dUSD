{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      inherit (purs-nix) ps-pkgs;
      inherit (config.ps) ctl-pkgs;
      inherit (config) offchain-lib;

      local-ctl-runtime = {
        dependencies =
          with ps-pkgs;
          [
            aff
            bigints
            console
            ctl-pkgs.cardano-transaction-lib
            effect
            optparse
            prelude
            tailrec
            uint
          ];
        ps =
          purs-nix.purs
            {
              inherit (local-ctl-runtime) dependencies;
              dir = ./.;
            };
        package =
          let js = "${local-ctl-runtime.ps.modules.Main.output {}}/Main/index.js"; in
          pkgs.writeScriptBin "local-ctl-runtime"
            ''
              export NODE_PATH=${config.ctl.nodeModules}/node_modules
              export PATH=${pkgs.lib.makeBinPath [
                self.inputs.cardano-transaction-lib.packages.${pkgs.system}."ctl-server:exe:ctl-server"
                self.inputs.cardano-transaction-lib.inputs.plutip.packages.${pkgs.system}."plutip:exe:plutip-server"
                pkgs.postgresql
                self.inputs.mlabs-ogmios.defaultPackage.${pkgs.system}
                self.inputs.ogmios-datum-cache.defaultPackage.${pkgs.system}
              ]}
              ${pkgs.nodejs}/bin/node \
                --preserve-symlinks \
                --input-type=module \
                -e 'import { main } from "${js}"; main()' \
                -- "local-ctl-runtime" "''$@"
            '';
      };
    in
    {
      apps."ctl-runtime:local" = {
        type = "app";
        program = "${self'.packages."offchain:local-ctl-runtime"}/bin/local-ctl-runtime";
      };

      packages."offchain:local-ctl-runtime" = local-ctl-runtime.package;

      devShells."offchain:local-ctl-runtime" =
        offchain-lib.makeProjectShell local-ctl-runtime { };
    };
  flake = { };
}
