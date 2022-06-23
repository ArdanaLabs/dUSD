"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js
import("./output/Main/index.js").then((m) => {
    console.log(m.hello)
    m.hello("hello from JS")
});
// purs-nix automaticly adds the main() call

console.log("app starting");
