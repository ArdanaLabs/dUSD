{
  lib,
  self,
  ...
}: let
  inherit
    (lib)
    mkOption
    types
    ;
in {
  perSystem = system: {
    config,
    self',
    inputs',
    ...
  }: {
    options = {
      dusd = {
        lib = mkOption {
          type = types.uniq (types.attrsOf types.unspecified);
          description = ''
            dusd-lib contains helper functions for dealing with haskell.nix.
            From it, we inherit fixHaskellDotNix and some common attributes
            to give to cabalProject'.
          '';
          default = import "${self}/nix/lib/haskell.nix" {
            inherit system self;
            inherit (config.haskell-nix) pkgs;
          };
        };
      };
    };
  };
}
