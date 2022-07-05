module Test.Main
  (main
  ) where

import Prelude
import Effect (Effect)
import Test.Encoding as Encoding
import Effect.Aff (launchAff_)
import Test.Spec(it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Encoding.spec
  it "should fail" $ 0 `shouldEqual` 1