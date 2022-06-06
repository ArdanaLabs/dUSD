{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies = [ "aeson", "aff", "bigints", "cardano-transaction-lib" ]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "test/**/*.purs" ]
}
