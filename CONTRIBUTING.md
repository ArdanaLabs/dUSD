## Policies

### Flake Inputs

In the `master` branch of this repository, Flake inputs should only reference
`master`, `main` or release tags/branches. Forks, specific refs or feature
branches that are being awaited upstream should not be used in `master`, without
considerable review or discussion.

Good ✔
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    plutus.url = "github:input-output-hk/plutus/v1.0.0";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };
}
```
Bad ✖
```nix
{
  inputs = {
    nixpkgs.url = "github:matthewcroughan/nixpkgs/5dc2ba25fdf43e748db617c2f9f58787a05c7cf4";
    plutus.url = "github:input-output-hk/plutus/yetToBeMergedFeature";
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
  };
}
```
