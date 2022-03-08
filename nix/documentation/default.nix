{ writeShellApplication
, entr
, texlive
}:
let
  latexEnv = texlive.combine { inherit ( texlive ) scheme-basic latexmk; };
in
writeShellApplication
  {
    name = "feedback-loop";
    runtimeInputs = [ entr latexEnv ];
    text =
      ''
      cd nix/documentation
      echo "test-plan.tex" | entr latexmk -pdf test-plan.tex
      '';
  }
