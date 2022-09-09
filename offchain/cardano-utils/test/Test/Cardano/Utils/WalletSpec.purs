module Test.Cardano.Utils.WalletSpec (spec) where

import Effect.Aff
import Prelude

import Cardano.Utils.Wallet (generateKeyPair)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Warn))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Data.UInt as UInt
import Plutip.Server (withPlutipContractEnv)
import Plutip.Types (PlutipConfig)
import QueryM (QueryEnv(runtime))
import Test.Spec (Spec, it, describe, aroundWith)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = do
  testGenerateKeyPair

withTest :: ( Int -> Int -> Aff Unit) -> Aff Unit
withTest = undefined

testGenerateKeyPair :: Spec Unit
testGenerateKeyPair = do
  describe "generateKeyPair" do
    it "" $ \_ -> do
      withPlutipContractEnv plutipConfig [ BigInt.fromInt 10_000_000 ] $ \env _ -> do
        let x = runtime env
        1 `shouldEqual` 1


plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8084
  , logLevel: Warn
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
