{self, ...}: {
  perSystem = system: {
    config,
    self',
    inputs',
    ...
  }: let
    # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
    # packages once, so we can reuse it here, it's more performant.
    pkgs = config.haskell-nix.pkgs;
    # dusd-lib contains helper functions for dealing with haskell.nix. From it,
    # we inherit fixHaskellDotNix and some common attributes to give to
    # cabalProject'. The library is defined in a flake-module in
    # nix/flake-modules/dusd-lib, to make it more performant and convenient
    # in reuses
    inherit
      (config.dusd.lib)
      commonPlutusModules
      commonPlutusShell
      fixHaskellDotNix
      ;

    haskellNixFlake =
      fixHaskellDotNix (project.flake {})
      [./dUSD-offchain.cabal ./hello-world/hello-world.cabal];

    onchain-scripts = self'.packages.onchain-scripts;
    subdir = "offchain";
    project = let
      cardanoNodePackages = self.inputs.cardano-node.outputs.packages.${system};
      cardanoNode = cardanoNodePackages."cardano-node:exe:cardano-node";
      cardanoNodeCli = cardanoNodePackages."cardano-cli:exe:cardano-cli";
      shelleyTestData = "${self.inputs.plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley";
    in
      pkgs.haskell-nix.cabalProject' {
        src = pkgs.runCommandNoCC "fakesrc-offchain" {} ''
          cp -rT ${./.} $out && cd $out
          chmod u+w cabal.project
          cat cabal-haskell.nix.project >> cabal.project
        '';
        compiler-nix-name = "ghc8107";
        cabalProjectFileName = "cabal.project";
        shell =
          commonPlutusShell
          // {
            additional = ps:
              with ps; [
                plutus-contract
                plutus-ledger
                plutus-ledger-api
                plutus-ledger-constraints
                plutus-pab
                plutus-use-cases
              ];
            DUSD_SCRIPTS = onchain-scripts;
            propagatedBuildInputs = [
              # cardano-node and cardano-cli need to be on the PATH to run the
              # cluster + PAB.
              cardanoNode
              cardanoNodeCli
            ];
            tools = {
              ghcid = {};
              haskell-language-server = {};
            };
          };
        modules =
          commonPlutusModules
          ++ [
            {
              packages = {
                hello-world.components.library.DUSD_SCRIPTS = onchain-scripts;
                hello-world.components.exes.hello-world-cluster = {
                  pkgconfig = [pkgs.makeWrapper];
                  postInstall = ''
                    wrapProgram $out/bin/hello-world-cluster \
                      --set 'SHELLEY_TEST_DATA' '${shelleyTestData}' \
                      --prefix PATH : "${pkgs.lib.makeBinPath [cardanoNode cardanoNodeCli]}"
                  '';
                };
                dUSD-offchain.components.tests.tests = {
                  pkgconfig = [pkgs.makeWrapper];
                  postInstall = ''
                    wrapProgram $out/bin/tests \
                      --set 'DUSD_SCRIPTS' '${onchain-scripts}'
                  '';
                };
                hello-world.components.tests.hello-world-e2e = {
                  pkgconfig = [pkgs.makeWrapper];
                  postInstall = ''
                    wrapProgram $out/bin/hello-world-e2e \
                      --set 'SHELLEY_TEST_DATA' '${shelleyTestData}' \
                      --prefix PATH : "${pkgs.lib.makeBinPath [cardanoNode cardanoNodeCli]}"
                  '';
                };
              };
            }
          ];
        sha256map = import ./sha256map;
        pkg-def-extras = [
          (hackage: {
            packages = {
              cryptostore = hackage."cryptostore"."0.2.1.0".revisions.default;
              jwt = hackage."jwt"."0.11.0".revisions.default;
              random = hackage.random."1.2.1".revisions.default;
            };
          })
        ];
      };
  in {
    devShells.offchain = haskellNixFlake.devShell;
    packages = haskellNixFlake.packages;
  };
  flake = {};
}
