{
  description = "dUSD";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "spec-type";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      haskell-nix,
      plutus,
      lint-utils
    }
    @ inputs:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" ]; #"aarch64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system: let
        deferPluginErrors = true;
        pkgs = nixpkgsFor system;

        fakeSrc = pkgs.runCommand "real-source" { } ''
          cp -rT ${self} $out
          chmod u+w $out/cabal.project
          cat $out/cabal-haskell.nix.project >> $out/cabal.project
        '';
      in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [
            {
              packages = {
                marlowe.flags.defer-plugin-errors = deferPluginErrors;
                plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                cardano-crypto-praos.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                cardano-crypto-class.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              };
            }
          ];

          pkg-def-extras = [
            (hackage: {
              packages = {
                random = (((hackage.random)."1.2.1").revisions).default;
              };
            })
          ];

          shell = {
            withHoogle = true;
            tools = {
              haskell-language-server = { };
            };

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [ pkgs.cabal-install pkgs.hlint pkgs.haskellPackages.fourmolu ];

            additional = ps: [
              ps.apropos
              ps.apropos-tx
              ps.plutarch
              ps.plutus-contract
              ps.plutus-ledger
              ps.plutus-ledger-api
              ps.plutus-ledger-constraints
              ps.sydtest
              ps.sydtest-hedgehog
            ];
          };
          sha256map = {
            "https://github.com/mlabs-haskell/apropos"."455b1a3ad1eee35de4d3fb8c4a4527071474336c" = "sha256-EC6vnimXA+jBRPQLLs2dltuTx9XoSdkQfh742NnLJSQ=";
            "https://github.com/mlabs-haskell/apropos-tx"."d5a90656ad77a48d2291748e1bb5ae072c85eaa4" = "sha256-SkWvW7EyI94BoFWvzyk+MsTNd3eomRlwaBovIQtI71o=";
            "https://github.com/Plutonomicon/plutarch"."aecc2050eb63ff0041576473aa3193070fe91314" = "sha256-pVvSa4fBoKXCdCu/NGduoKhr1/gGESCmj/Tr9Y5l9B4=";
            "https://github.com/input-output-hk/plutus.git"."6c580c150b8e8afdf14e43a234fcc8db47e3c1d2" = "sha256-dwDHhOAGAxRksTMozocxgmBncToLb5HOSD8lKe/UCYE=";
            "https://github.com/input-output-hk/plutus-apps"."c4960ae14b187978bf41832313370d282f648eee" = "sha256-6aQXgtTzrp45uErLm2Uo67Tu7vkDT5w01ebe0wdi+y8=";
            "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/cardano-addresses"."5a313b60ed64e4374095de65bc13cb080001e520" = "sha256-K7j84d9UzUDH3aekpH5IMXyUpG1ciIfb2t2+0o9VHKI=";
            "https://github.com/input-output-hk/cardano-config"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6" = "sha256-jQbwcfNJ8am7Q3W+hmTFmyo3wp3QItquEH//klNiofI=";
            "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
            "https://github.com/input-output-hk/cardano-base"."5c1786f3a2b9b2647489862963003afdc1f27818" = "sha256-cMQjyQDdHQvZwc9MIJ+cPyxFW0rEPPidEytAed5IZns=";
            "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" = "sha256-lRNfkGMHnpPO0T19FZY5BnuRkr0zTRZIkxZVgHH0fys=";
            "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d" = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
            "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
            "https://github.com/input-output-hk/cardano-wallet"."a5085acbd2670c24251cf8d76a4e83c77a2679ba" = "sha256-A3im2IkoumUx3NzgPooaXGC18/iYxbEooMa9ho93/6o=";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "sha256-z9ut0y6umDIjJIRjz9KSvKgotuw06/S8QDwOtVdGiJ0=";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "sha256-QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "sha256-uQx+SEYsCH7JcG3xAT0eJck9yq3y0cvx49bvItLLer8=";
            "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132" = "sha256-fZ6FfG2z6HWDxjIHycLPSQHoYtfUmWZOX7lfAUE+s6M=";
            "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "sha256-DH9ISydu5gxvN4xBuoXVv1OhYCaqGOtzWlACdJ0H64I=";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
            "https://github.com/Srid/sydtest"."9c6c7678f7aabe22e075aab810a6a2e304591d24" = "sha256-P6ZwwfFeN8fpi3fziz9yERTn7BfxdE/j/OofUu+4GdA=";
            "https://github.com/Srid/autodocodec"."42b42a7407f33c6c74fa4e8c84906aebfed28daf" = "sha256-X1TNZlmO2qDFk3OL4Z1v/gzvd3ouoACAiMweutsYek4=";
            "https://github.com/Srid/validity"."f7982549b95d0ab727950dc876ca06b1862135ba" = "sha256-dpMIu08qXMzy8Kilk/2VWpuwIsfqFtpg/3mkwt5pdjA=";
          };
        };

        lintSpec = {
          cabal-fmt = {};
          fourmolu = {
            ghcOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
          };
          # Enable after https://github.com/ArdanaLabs/dUSD/issues/8
          # nixpkgs-fmt = {};
        };
    in
      {
        project = forAllSystems projectFor;
        flake = forAllSystems (system: (projectFor system).flake { });

        # this could be done automatically, but would reduce readability
        packages = forAllSystems (system:
        let pkgs = (forAllSystems nixpkgsFor)."${system}";
        in self.flake.${system}.packages // {
          build-docs = pkgs.stdenv.mkDerivation {
            name = "test-plan";
            src = self;
            buildInputs = with pkgs; [ (texlive.combine { inherit ( texlive ) scheme-basic latexmk todonotes metafont; }) ];
            doCheck = false;
            buildPhase = ''
              HOME=$TMP latexmk -output-directory="tmp" -pdf ./docs/*.tex
              mkdir $out -p
              cp tmp/*.pdf $out
            '';
            installPhase = ''
              ls -lah
            '';
          };
        });
        checks = forAllSystems (system:
          self.flake.${system}.checks //
            (lint-utils.mkChecks.${system} lintSpec ./.)
        );
        # We need this attribute because `nix flake check` won't work for Haskell
        # projects: https://nixos.wiki/wiki/Import_From_Derivation#IFD_and_Haskell
        #
        # Instead, run: `nix build .#check.x86_64-linux` (replace with your system)
        check = forAllSystems (
          system:
            (nixpkgsFor system).runCommand "combined-test" {
              nativeBuildInputs = builtins.attrValues self.checks.${system};
            } "touch $out"
        );

        devShell = forAllSystems (system: self.flake.${system}.devShell);
        defaultPackage = forAllSystems (system: self.packages.${system}."dUSD:test:tests");
        apps = forAllSystems (system: let
          pkgs = (forAllSystems nixpkgsFor)."${system}";
        in
          {
            feedback-loop = {
              type = "app";
              program = "${ pkgs.callPackage ./nix/apps/feedback-loop { } }/bin/feedback-loop";
            };
            format =  lint-utils.mkApp.${system} lintSpec;
          });
      };
}
