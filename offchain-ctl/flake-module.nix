{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      pkgs = import self.inputs.nixpkgs {
                  inherit system;
                  overlays = [ self.inputs.cardano-transaction-lib.overlay.${system} ];
                };
      offchainPsProject = pkgs.purescriptProject {
        inherit pkgs;
        projectName = "offchain-ctl";
        src = ./.;
      };
      hello-world-api = offchainPsProject.bundlePursProject {
        sources = [ "hello-world-api/src" ];
        main = "Main";
      };
    in
    {
      packages = {
        inherit hello-world-api;
        ctl-scaffold-runtime = pkgs.buildCtlRuntime { };
      };
      apps = {
        ctl-scaffold-runtime = pkgs.launchCtlRuntime { };
      };
      devShells.offchain-ctl = offchainPsProject.devShell;
    };
  flake = {
  };
}