{ self, lib, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      purs-nix = config.ps.purs-nix;
      all-ps-pkgs = config.ps.pkgs;
      inherit (config) dusd-lib offchain-lib;

      # Ideally we would just append the CTL overlay to the haskell-nix pkgs
      # we already have at `config.haskell-nix.pkgs`, but our haskell-nix
      # instances seem to be incompatible. So we just use CTLs haskell-nix here.
      ctl-pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = with self.inputs.cardano-transaction-lib; [
          inputs.haskell-nix.overlay
          inputs.iohk-nix.overlays.crypto
          overlays.runtime
        ];
      };

      # use more recent slot to avoid long sync time
      ctlRuntimeConfig = {
        datumCache.blockFetcher.firstBlock = {
          slot = 62153233;
          id = "631c621b7372445acf82110282ba72f4b52dafa09c53864ddc2e58be24955b2a";
        };
      };

      hello-world-cbor =
        purs-nix.build
          {
            name = "hello-world-cbor";
            src.path = self'.packages."onchain:hello-world-cbor-purs";
            info.dependencies = [ ];
            info.version = "0.0.1";
          };
    in
    {
      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime ctlRuntimeConfig;
        "offchain:docs:serve" =
          dusd-lib.makeServeApp
            "${self'.packages."offchain:docs"}/html/";
        "offchain:test" =
          let
            getTestScript = outputName:
              self'.apps."offchain:${outputName}:test".program;
            runTests =
              pkgs.writeScript "run-tests" ''
                # --will-cite gets rid of the annoying citation notice
                # we disable the shellcheck that says things wont expand in singlequote
                # because it's a false positive
                # shellcheck disable=SC2016
                ${pkgs.parallel}/bin/parallel --will-cite \
                  '. {} &> "$(basename {.})-output"' ::: \
                  ${getTestScript "hello-world-api"} \
                  ${getTestScript "hello-world-browser"} \
                  ${self'.apps."offchain:hello-world-cli:test:local".program}
                printf "$?" > "$TEST_EXITCODE_FILE"
              '';
          in
          dusd-lib.mkApp (
            pkgs.writeShellApplication
              {
                name = "offchain-test-all";
                runtimeInputs = with pkgs; [ coreutils psutils ncurses ];
                text = ''
                  shopt -s nullglob
                  # create a file to store parallel exit code in
                  TEST_EXITCODE_FILE="$(mktemp)"

                  # run tests in background
                  export TEST_EXITCODE_FILE
                  ${runTests} &

                  # get our parallel command PID
                  TEST_PID=$!
                  # set a trap so if we CTRL+C the script test command is killed
                  trap 'kill $TEST_PID' EXIT

                  function print_logs {
                    for file in "''${outfiles[@]}"; do
                      echo -e "$file: $(grep . "$file" | tail -qn 1)"
                    done
                  }

                  # print if parallel is still running
                  while true; do
                    outfiles=(*-output)
                    print_logs
                    # sleep 1 second between updates
                    sleep 1
                    # clear the amount of lines we printed
                    tput cuu ''${#outfiles[@]}
                    tput ed
                    # check if the test command still running or not
                    if ! ps -p $TEST_PID > /dev/null; then
                      print_logs
                      break
                    fi
                  done

                  # remove trap since here the test command will have exit already
                  trap - EXIT
                  # exit with the exitcode of our test command
                  exit "$(cat "$TEST_EXITCODE_FILE")"
                '';
              }
          );
      };
      packages = {
        "offchain:hello-world-cbor" = hello-world-cbor;
        "offchain:docs" =
          pkgs.runCommand "offchain-all-docs" { }
            ''
              mkdir -p $out
              # link hello-world-api docs
              ln -sf ${self'.packages."offchain:hello-world-api:docs"}/generated-docs/html $out/html
              if ! [ -f "$out/html/index.html" ]; then
                echo "doc generation did not create index.html in the expected location"
                exit 1
              fi
            '';
      };
    };

  flake = {
    ctl-qemu-image =
      let
        nixpkgsSource = self.inputs.nixpkgs.sourceInfo.outPath;
        system = "x86_64-linux";
        pkgs = self.inputs.nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        name = "ogmios-datum-cache";
      in
      import (nixpkgsSource + "/nixos/lib/make-disk-image.nix") {
        inherit pkgs lib;
        format = "qcow2";
        diskSize = "auto";
        additionalSpace = "20000M";
        config = (import (nixpkgsSource + "/nixos/lib/eval-config.nix") {
          inherit pkgs system;
          modules = [
            self.inputs.nixpkgs.nixosModules.notDetected
            (nixpkgsSource + "/nixos/modules/profiles/qemu-guest.nix")
            ({ pkgs, config, ... }: {
              fileSystems."/".device = "/dev/disk/by-label/nixos";
              boot.loader.grub.device = "/dev/vda";
              boot.loader.timeout = 0;
              users.extraUsers.root.password = "";
              system.stateVersion = "22.05";
              imports = [
                self.nixosModules.ctl-server
                self.nixosModules.ogmios-datum-cache
                self.inputs.cardano-ogmios.nixosModules.ogmios
                self.inputs.cardano-node.nixosModules.cardano-node
              ];

              networking.firewall.allowedTCPPorts = [
                config.services.cardano-node.port
                config.services.ctl-server.port
                config.services.cardano-ogmios.port
                config.services.ogmios-datum-cache.port
              ];

              services.postgresql = {
                enable = true;
                ensureDatabases = [ name ];
                ensureUsers = [
                  {
                    inherit name;
                    ensurePermissions = {
                      "DATABASE \"${name}\"" = "ALL PRIVILEGES";
                    };
                  }
                ];
              };

              systemd.services.cardano-node.serviceConfig.Type = "simple";

              services.cardano-node = {
                enable = true;
                environment = "testnet";
                nodeConfigFile = "${self.inputs.cardano-node}/configuration/cardano/${config.services.cardano-node.environment}-config.json";
                topology = "${self.inputs.cardano-node}/configuration/cardano/${config.services.cardano-node.environment}-topology.json";
                extraServiceConfig = i: {
                  serviceConfig.ExecStartPost = pkgs.writeShellScript "change-cardano-node-socket-permissions" ''
                    while [ ! -S ${config.services.cardano-node.socketPath} ]; do
                      sleep 1
                    done

                    chmod 0666 ${config.services.cardano-node.socketPath}
                  '';
                };
              };

              services.cardano-ogmios = {
                enable = true;
                nodeConfig = "${self.inputs.cardano-node}/configuration/cardano/testnet-config.json";
                nodeSocket = config.services.cardano-node.socketPath;
              };

              services.ogmios-datum-cache = {
                enable = true;
                postgresql = {
                  user = name;
                  dbName = name;
                };
              };

              services.ctl-server.enable = true;
            })
          ];
        }).config;
      };
  };
}
