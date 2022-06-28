{ self, ... }:
{
  perSystem = system: { config, self', inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      projectName = "hello-world";
      purs-nix = self.inputs.purs-nix-0-14 { inherit system; };
      npmlock2nix = pkgs.callPackages self.inputs.npmlock2nix {};

      hello-world-api = {
        dependencies = 
          with purs-nix.ps-pkgs;
            [ aeson
              aff
              bigints
              cardano-transaction-lib
            ];
        ps =
          purs-nix.purs
            { inherit (hello-world-api) dependencies;
              srcs = [ ./hello-world-api/src ];
            };
        package = (purs-nix.build
          { name = "hello-world-api";
            inherit (hello-world-api) dependencies;
            repo = ""; rev = "";
          }).local ./hello-world-api;
      };
      
      hello-world-browser = {
        ps =
          purs-nix.purs
            { dependencies =
                with purs-nix.ps-pkgs;
                [ cardano-transaction-lib
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-browser/src ];
            };
      };

      hello-world-cli = {
        ps =
          purs-nix.purs
            { dependencies =
                with purs-nix.ps-pkgs;
                [ prelude
                  hello-world-api.package
                ];
              srcs = [ ./hello-world-browser/src ];
            };
      };

      ctl-pkgs = import self.inputs.nixpkgs {
                   inherit system;
                   overlays = [ self.inputs.cardano-transaction-lib.overlay.${system} ];
                 };
      # use more recent slot to avoid long sync time
      config = {
        datumCache.blockFetcher.firstBlock = {
          slot = 60854917;
          id = "1c157a01120772c35d468b425e6ef228d5b6cec5977f7897540aa8d0870f9ab9";
        };
      };
    in
    {
      packages.hello-world-api = hello-world-api.package;

      # packages.hello-world-browser =
      #   pkgs.runCommand "build-hello-world-browser" { }
      #   # see buildPursProjcet: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L74
      #   # see bundlePursProject: https://github.com/Plutonomicon/cardano-transaction-lib/blob/c906ead97563fef3b554320bb321afc31956a17e/nix/default.nix#L149
      #   ''
      #   mkdir $out; cd $out
      #   export BROWSER_RUNTIME=1
      #   cp ${hello-world-browser.ps.modules.Main.bundle {main = true;} } output.js
      #   cp ${./hello-world-browser/index.js} index.js
      #   cp ${./hello-world-browser/index.html} index.html
      #   cp ${./webpack.config.js} webpack.config.js
      #   cp -r ${npmlock2nix.node_modules { src = ./.; }}/* .
      #   export NODE_PATH="node_modules"
      #   export PATH="bin:$PATH"
      #   mkdir dist
      #   webpack --mode=production -c webpack.config.js -o ./dist --entry ./index.js
      #   '';

      # packages.hello-world-cli = hello-world-browser.ps.modules.Main.bundle {main = true;};

      apps = {
        ctl-runtime = ctl-pkgs.launchCtlRuntime config;

        # serve-hello-world-browser = {
        #   type = "app";
        #   program = pkgs.writeShellApplication
        #     {
        #       name = projectName;
        #       runtimeInputs = [
        #         pkgs.nodePackages.http-server
        #       ];
        #       text = "http-server -c-1 ${self'.packages.hello-world-browser}";
        #     };
        # };
      };

      devShells.hello-world-browser = pkgs.mkShell {
        name = projectName;
        buildInputs = (with pkgs; [
          nodejs-16_x
          # (hello-world-browser.ps.command {})
          purs-nix.ps-pkgs.psci-support
          purs-nix.purescript
          purs-nix.purescript-language-server
          nodePackages.purs-tidy
        ]);
        shellHook = "export NODE_PATH=${npmlock2nix.node_modules { src = ./.; }}/node_modules/";
      };
    };
  flake = {
  };
}
