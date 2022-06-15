## How to build and run

To build the bundle run:
```
nix build .#packages.x86_64-linux.hello-world-api
```

To try out the app after building do:
```
nix run .#apps.x86_64-linux.ctl-scaffold-runtime
nix shell nixpkgs#nixpkgs#nodePackages.http-server

http-server -c-1 result/dist/
```