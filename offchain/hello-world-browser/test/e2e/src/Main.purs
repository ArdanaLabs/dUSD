module HelloWorld.Test.E2E.Main where

import Contract.Prelude

import Contract.Test.Plutip (PlutipConfig)
import Control.Parallel (parTraverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (closeStaticServer, startStaticServer)
import HelloWorld.Test.E2E.TestPlans.Increment as TestPlanIncrement
import HelloWorld.Test.E2E.TestPlans.Initialize as TestPlanInitialize
import HelloWorld.Test.E2E.TestPlans.Redeem as TestPlanRedeem
import HelloWorld.Test.E2E.TestWallet (mkTestOptions, testWallet1, testWallet2, testWallet3, topup)
import Mote (group)
import Node.Process (lookupEnv)
import Plutip.Server (startPlutipCluster, startPlutipServer, stopChildProcessWithPort, stopPlutipCluster)
import Test.Spec.Runner as SpecRunner
import Utils as Utils

main ∷ Effect Unit
main = do
  helloWorldBrowserIndex <- lookupEnv Env.helloWorldBrowserIndex

  case helloWorldBrowserIndex of
    Nothing -> throw "HELLO_WORLD_BROWSER_INDEX not set"
    Just index -> launchAff_ do
      bracket (startPlutipServer config) (stopChildProcessWithPort config.port) $ const do
        bracket (startPlutipCluster config unit) (\_ -> void $ stopPlutipCluster config) $ const do
          bracket (startStaticServer index) closeStaticServer $ \_ -> do
            wallet1 <- liftEffect testWallet1
            wallet2 <- liftEffect testWallet2
            wallet3 <- liftEffect testWallet3

            parTraverse_ (liftEffect <<< topup) [ wallet1, wallet2, wallet3 ]

            testOptions1 <- liftEffect $ mkTestOptions wallet1
            testOptions2 <- liftEffect $ mkTestOptions wallet2
            testOptions3 <- liftEffect $ mkTestOptions wallet3

            Utils.interpret'
              (SpecRunner.defaultConfig { timeout = pure $ wrap Constants.specRunnerTimeoutMs })
              ( group "Nami wallet" do
                  TestPlanInitialize.testPlan testOptions1
                  TestPlanIncrement.testPlan testOptions2
                  TestPlanRedeem.testPlan testOptions3
              )

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }