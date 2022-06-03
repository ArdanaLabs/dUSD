## Run the tests

```console
$ nix build -L .#hello-world-ui-selenium-test:test:sydtest-webdriver
```

## Run the tests in devleopment env

```console
$ nix develop .#hello-world-ui-selenium-test
$ cd offchain/hello-world-ui/test/selenium
$ cabal test
```
