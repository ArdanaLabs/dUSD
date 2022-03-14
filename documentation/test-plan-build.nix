{ stdenv
, entr
, texlive
}:
let
  latexEnv = texlive.combine { inherit ( texlive ) scheme-basic latexmk; };
in
stdenv.mkDerivation
  {
    name = "test-plan-documentation";
    src = ./.;
    buildInputs = [ entr latexEnv ];
    buildPhase =
      ''
      mkdir -p $out
      latexmk -pdf test-plan.tex
      cp test-plan.pdf "$out/test-plan.pdf"
      '';

      installPhase = ''
        echo done
      '';
  }
