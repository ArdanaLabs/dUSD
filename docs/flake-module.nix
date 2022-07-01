{self, ...}: {
  perSystem = system: {
    config,
    self',
    inputs',
    ...
  }: let
    pkgs = inputs'.nixpkgs.legacyPackages;
  in {
    packages = {
      build-docs = pkgs.stdenv.mkDerivation {
        name = "build-docs";
        src = ./.;

        buildInputs = with pkgs; [
          (texlive.combine {
            inherit
              (texlive)
              scheme-basic
              latexmk
              todonotes
              metafont
              ;
          })
        ];

        doCheck = false;
        doConfigure = false;

        buildPhase = ''
          export HOME="$TMP"
          latexmk -output-directory=. -pdf ./*.tex
        '';
        installPhase = ''
          mkdir -p $out
          mv ./*.pdf $out
        '';
      };
    };
    apps = {
      feedback-loop = {
        type = "app";
        program = pkgs.writeShellApplication {
          name = "dusd-feedback-loop";
          runtimeInputs = [pkgs.entr];
          text = "find docs -name '*.tex' | entr nix build .#build-docs";
        };
      };
    };
  };
  flake = {};
}
