module Main (main) where

import Contract.Prelude (LogLevel(Error))
import Contract.Test.Plutip (withPlutipContractEnv, PlutipConfig)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (logShow)
import Effect.Class (liftEffect)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, int, long, metavar, option, progDesc, short, value, (<**>))
import Wallet.Key (keyWalletPrivatePaymentKey)
import Wallet.KeyFile (formatPaymentKey)

import Prelude

main :: Effect Unit
main =
  execParser opts >>= \(LocalCtlRuntimeOptions initialAdaValue) -> do
    launchAff_ $ withPlutipContractEnv config [ initialAdaValue ] \_ wallets -> do
        let key = (fromMaybe "Couldn't format the private payment key" <<< formatPaymentKey <<< keyWalletPrivatePaymentKey) $ wallets
        liftEffect $ logShow key

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

data LocalCtlRuntimeOptions = LocalCtlRuntimeOptions BigInt.BigInt

derive instance genericLocalRuntimeOptions :: Generic LocalCtlRuntimeOptions _
instance showLocalRuntimeOptions :: Show LocalCtlRuntimeOptions where
  show = genericShow

opts :: ParserInfo LocalCtlRuntimeOptions
opts = 
  info
    (localCtlRuntimeOptions <**> helper)
    (fullDesc
    <> progDesc "Run the CTL runtime with a local testnet (enabled by plutip)"
    <> header "local-ctl-runtime")

localCtlRuntimeOptions :: Parser LocalCtlRuntimeOptions
localCtlRuntimeOptions = LocalCtlRuntimeOptions <$> initialAdaValueParser

initialAdaValueParser :: Parser BigInt.BigInt
initialAdaValueParser = option (BigInt.fromInt <$> int)
    (  long "initial-ada"
    <> short 'i'
    <> metavar "INT"
    <> value (BigInt.fromInt 100_000_000)
    <> help "The initial funding in lovelace for the test wallet")
