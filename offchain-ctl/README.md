## How to build and run

To build the bundle run:
```
nix build .#packages.x86_64-linux.offchain-ctl
```

To try out the app do:
```
nix build .#packages.x86_64-linux.offchain-ctl
nix shell nixpkgs#nixpkgs#nodePackages.http-server

http-server -c-1 result/dist/
```