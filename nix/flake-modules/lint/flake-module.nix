{ self, ... }:
{
  perSystem = { config, self', inputs', system, ... }:
    let
      # TODO: We should be able to get rid of this once
      # https://github.com/numtide/treefmt/issues/165 is implemented upstream.
      # Uses https://github.com/numtide/treefmt to provide auto-formating for multiple
      # languages. Configure the behaviour in treefmt.toml at project root.
      pkgs = inputs'.nixpkgs.legacyPackages;
      dependencies = with pkgs; [
        haskellPackages.hlint
        # haskellPackages.fourmolu
        # treefmt
        # nixpkgs-fmt
        nodePackages.purs-tidy
      ];
    in
    {
      apps = {
        # Lints .hs files.
        lint = {
          type = "app";
          program = pkgs.writeShellApplication {
            name = "lint";
            runtimeInputs = dependencies;
            text = "hlint .";
          };
        };
      };
      checks = { };
      # Old
      # lint = pkgs.runCommandLocal "lint-check"
      #   {
      #     buildInputs = dependencies;
      #   } ''
      #   # set -e
      #   # export HOME="$TMP"
      #   # treefmt -vvv --no-cache --fail-on-change -C ${self}
      #   # touch $out
      #   echo ${self}
      # '';
      # };
    };
  flake = { };
}
