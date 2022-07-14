{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      # dusd-lib contains helper functions for dealing with haskell.nix. From it,
      # we inherit fixHaskellDotNix and some common attributes to give to
      # cabalProject'
      dusd-lib = import "${self}/nix/lib/haskell.nix" { inherit system self pkgs; };
      inherit (dusd-lib) commonPlutusModules commonPlutusShell fixHaskellDotNix;

      haskellNixFlake =
        fixHaskellDotNix (project.flake { })
          [ ./dUSD-offchain.cabal ./hello-world/hello-world.cabal ];

      onchain-scripts = self'.packages.onchain-scripts;
      subdir = "offchain";
      # Checks the shell script using ShellCheck
      checkedShellScript = system: name: text:
        (pkgs.writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";
      project = pkgs.haskell-nix.cabalProject' {
        src = pkgs.runCommand "fakesrc-offchain" { } ''
          cp -rT ${./.} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';
        compiler-nix-name = "ghc8107";
        cabalProjectFileName = "cabal.project";
        shell = commonPlutusShell // {
          additional = ps: [
            ps.plutus-contract
            ps.plutus-ledger
            ps.plutus-ledger-api
            ps.plutus-ledger-constraints
            ps.plutus-pab
            ps.plutus-use-cases
          ];
          DUSD_SCRIPTS = onchain-scripts;
          propagatedBuildInputs = [
            # cardano-node and cardano-cli need to be on the PATH to run the
            # cluster + PAB.
            self.inputs.cardano-node.outputs.packages.${system}."cardano-node:exe:cardano-node"
            self.inputs.cardano-node.outputs.packages.${system}."cardano-cli:exe:cardano-cli"
          ];
          tools = {
            ghcid = { };
            haskell-language-server = { };
          };
        };
        modules = commonPlutusModules ++ [{
          packages = {
            hello-world.components.library.preBuild = "export DUSD_SCRIPTS=${onchain-scripts}";
            hello-world.components.exes.hello-world-cluster = {
              pkgconfig = [ [ pkgs.makeWrapper ] ];
              postInstall = with pkgs; ''
                wrapProgram $out/bin/hello-world-cluster \
                  --set 'SHELLEY_TEST_DATA' '${self.inputs.plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley' \
                  --prefix PATH : "${pkgs.lib.makeBinPath [
                    self.inputs.cardano-node.outputs.packages.${system}."cardano-node:exe:cardano-node"
                    self.inputs.cardano-node.outputs.packages.${system}."cardano-cli:exe:cardano-cli"
                  ]}"
              '';
            };
            dUSD-offchain.components.tests.tests = {
              pkgconfig = [ [ pkgs.makeWrapper ] ];
              postInstall = with pkgs; ''
                wrapProgram $out/bin/tests \
                  --set 'DUSD_SCRIPTS' '${onchain-scripts}'
              '';
            };
            hello-world.components.tests.hello-world-e2e = {
              pkgconfig = [ [ pkgs.makeWrapper ] ];
              postInstall = with pkgs; ''
                wrapProgram $out/bin/hello-world-e2e \
                  --set 'SHELLEY_TEST_DATA' '${self.inputs.plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley' \
                  --prefix PATH : "${pkgs.lib.makeBinPath [
                    self.inputs.cardano-node.outputs.packages.${system}."cardano-node:exe:cardano-node"
                    self.inputs.cardano-node.outputs.packages.${system}."cardano-cli:exe:cardano-cli"
                  ]}"
              '';
            };
          };
        }];
        sha256map = import ./sha256map;
        pkg-def-extras = [
          (hackage: {
            packages = {
              cryptostore = (((hackage."cryptostore")."0.2.1.0").revisions).default;
              jwt = (((hackage."jwt")."0.11.0").revisions).default;
              random = (((hackage.random)."1.2.1").revisions).default;
            };
          })
        ];
      };
    in
    {
      devShells.offchain = haskellNixFlake.devShell;
      packages = haskellNixFlake.packages // { };
    };
  flake = { };
}


