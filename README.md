# Feedback loops

You can set up a feedback loop through
```
cd nix/documentation
echo "test-plan.tex" | entr latexmk -pdf test-plan.tex
```
or simply by running
```
nix run .#feedback-loop
```
