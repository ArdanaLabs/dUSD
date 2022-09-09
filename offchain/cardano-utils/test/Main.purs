module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Cardano.Utils.WalletSpec as Test.Cardano.Utils.WalletSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)

main :: Effect Unit
main = do
  launchAff_ do
    runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
      Test.Cardano.Utils.WalletSpec.spec
