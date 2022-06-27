{
  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    cardano-node.url = "github:input-output-hk/cardano-node?rev=73f9a746362695dc2cb63ba757fbcabb81733d23";
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib?rev=6fca5ee2ff27f81762689c7da0d307e1afe7bbd3";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus";
    plutus-apps.url = "github:input-output-hk/plutus-apps?rev=e4062bca213f233cdf9822833b07aa69dff6d22a";
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-modules-core = {
      url = "github:hercules-ci/flake-modules-core";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    npmlock2nix = {
      flake = false;
      url = "github:nix-community/npmlock2nix";
    };
    purs-nix.url = "github:Geometer1729/purs-nix";
    purs-nix-0-14.url = "github:Geometer1729/purs-nix?rev=3fafe63040257b2740e7dfe5db311f16cbf89fd0";
  };

  outputs = { self, flake-modules-core, ... }:
    (flake-modules-core.lib.evalFlakeModule
      { inherit self; }
      {
        systems = [ "x86_64-linux" ];
        imports = [
          ./offchain-ctl
          ./offchain
          ./onchain
          ./docs
          ./nix/flake-modules
        ];
      }
    ).config.flake;
}
