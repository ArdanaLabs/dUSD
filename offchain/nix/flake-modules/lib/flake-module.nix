{ self, lib, ... }:
let
  inherit (lib)
    types
    mkOption
    ;
in
{
  perSystem = { config, self', inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      dusd-lib = config.dusd-lib;
      purs-nix = config.ps.purs-nix;
    in
    {
      options = {
        # These are some utilities we will use often in offchain nix code.
        offchain-lib = {
          makeProjectShell = mkOption {
            type = types.functionTo (types.functionTo types.package);
            description = ''
              Helper function to create a devshell without declaring common dependencies.
              If you want to add more dependencies, use `.overrideAttrs (old: { ... })`.
            '';
            default = project: cmdArgs:
              pkgs.mkShell {
                name = "hello-world";
                buildInputs = (with pkgs; [
                  nodejs-16_x
                  (project.ps.command cmdArgs)
                  purs-nix.ps-pkgs.psci-support
                  purs-nix.purescript
                  purs-nix.purescript-language-server
                  nodePackages.purs-tidy
                ]);
                shellHook = "export NODE_PATH=${config.ctl.nodeModules}/node_modules/";
              };
          };
        };
      };
    };
}