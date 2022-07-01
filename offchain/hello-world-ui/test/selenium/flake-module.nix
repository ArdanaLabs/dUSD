{self, ...}: {
  perSystem = system: {
    config,
    self',
    inputs',
    ...
  }: let
    description = "Selenium test for dUSD Hello World UI";
    # dusd-lib contains helper functions for dealing with haskell.nix. From it,
    # we inherit fixHaskellDotNix
    dusd-lib = import "${self}/nix/lib/haskell.nix" {inherit system self pkgs;};
    inherit (dusd-lib) fixHaskellDotNix;
    # realNixpkgs is required to get chromium and selenium from
    # cache.nixos.org rather than the bloated Haskell.nix Nixpkgs.
    realNixpkgs = inputs'.nixpkgs.legacyPackages;
    # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
    # packages once, so we can reuse it here, it's more performant.
    pkgs = config.haskell-nix.pkgs;
    haskellNixFlake =
      fixHaskellDotNix (project.flake {})
      [./hello-world-ui-selenium-test.cabal];
    project = pkgs.haskell-nix.cabalProject {
      modules = [
        {
          packages = {
            hello-world-ui-selenium-test.components.tests.sydtest-webdriver = {
              pkgconfig = [[realNixpkgs.makeWrapper]];
              postInstall = with realNixpkgs; ''
                wrapProgram $out/bin/sydtest-webdriver \
                  --set FONTCONFIG_FILE ${makeFontsConf {fontDirectories = [twitter-color-emoji roboto];}} \
                  --set HELLO_WORLD_UI_INDEX ${self'.packages.hello-world-ui} \
                  --prefix PATH : "${realNixpkgs.lib.makeBinPath [
                  chromedriver
                  chromium
                  selenium-server-standalone
                ]}"
              '';
            };
          };
        }
      ];
      name = "hello-world-ui-selenium-test";
      src = ./.;
      compiler-nix-name = "ghc8107";
      sha256map = import ./sha256map;
      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      shell = {
        tools = {
          cabal = {};
          hlint = {};
          haskell-language-server = {};
        };
        buildInputs = with realNixpkgs; [
          chromedriver
          chromium
          selenium-server-standalone
          nixpkgs-fmt
        ];
      };
    };
  in {
    packages = haskellNixFlake.packages;
    devShells.hello-world-ui-selenium-test = haskellNixFlake.devShell;
    checks = haskellNixFlake.checks // {};
  };
  flake = {};
}
