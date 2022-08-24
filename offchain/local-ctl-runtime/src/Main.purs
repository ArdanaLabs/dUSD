module Main (main) where

import Prelude

import Contract.Prelude (liftEffect)
import Contract.Test.Plutip (withPlutipContractEnv, PlutipConfig)
import Control.Monad.Rec.Class (forever)
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (toLower)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log, error)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, int, long, metavar, option, str, progDesc, short, value, (<**>))
import Wallet.Key (keyWalletPrivatePaymentKey)
import Wallet.KeyFile (formatPaymentKey)

-- add debug level options
-- add port options

main :: Effect Unit
main =
  execParser opts >>= \(LocalCtlRuntimeOptions lo) -> do
    launchAff_ $ withPlutipContractEnv (makeConfig $ LocalCtlRuntimeOptions lo) [ lo.initialAdaAmount ] \_ wallets -> do
        let key = (fromMaybe "Unable to format the received private payment key" <<< formatPaymentKey <<< keyWalletPrivatePaymentKey) $ wallets
        liftEffect <<< error $ "All CTL runtime dependencies started successfully."
        liftEffect <<< error $ "Private payment key funded with " <> BigInt.toString lo.initialAdaAmount
        liftEffect <<< log $ key
        liftEffect <<< error $ "Press Ctrl-C to stop the runtime"
        forever $ pure unit

makeConfig :: LocalCtlRuntimeOptions -> PlutipConfig
makeConfig (LocalCtlRuntimeOptions options) =
  { host: "127.0.0.1"
  , port: (options.plutipServerPort)
  , logLevel: options.plutipLogLevel
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: options.ogmiosPort
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: options.ogmiosDatumCachePort
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: options.ctlServerPort
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: options.postgresPort
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

newtype LocalCtlRuntimeOptions = LocalCtlRuntimeOptions {
  initialAdaAmount :: BigInt.BigInt
, plutipLogLevel :: LogLevel
, plutipServerPort :: UInt.UInt
, ogmiosPort :: UInt.UInt
, ogmiosDatumCachePort :: UInt.UInt
, ctlServerPort :: UInt.UInt
, postgresPort :: UInt.UInt
}
derive instance genericLocalRuntimeOptions :: Generic LocalCtlRuntimeOptions _

opts :: ParserInfo LocalCtlRuntimeOptions
opts = 
  info
    (localCtlRuntimeOptionsParser <**> helper)
    (fullDesc
    <> progDesc "Run the CTL runtime with a local testnet (enabled by plutip)"
    <> header "local-ctl-runtime")

localCtlRuntimeOptionsParser :: Parser LocalCtlRuntimeOptions
localCtlRuntimeOptionsParser =
  map LocalCtlRuntimeOptions $ { initialAdaAmount: _, plutipLogLevel: _, plutipServerPort: _, ogmiosPort: _, ogmiosDatumCachePort: _, ctlServerPort: _, postgresPort: _}
  <$> initialAdaValueParser
  <*> plutipLogLevelParser
  <*> makePortParser "plutip-server" (UInt.fromInt 8082)
  <*> makePortParser "ogmios" (UInt.fromInt 1338)
  <*> makePortParser "odc" (UInt.fromInt 10000)
  <*> makePortParser "ctl-server" (UInt.fromInt 8083)
  <*> makePortParser "postgres" (UInt.fromInt 5433)

initialAdaValueParser :: Parser BigInt.BigInt
initialAdaValueParser = option (BigInt.fromInt <$> int)
    (  long "initial-ada"
    <> short 'i'
    <> metavar "INT"
    <> value (BigInt.fromInt 100_000_000)
    <> help "The initial funding in lovelace for the test wallet")

plutipLogLevelParser :: Parser LogLevel
plutipLogLevelParser = option (logLevelFromString <$> str) (
     long "log-level"
  <> short 'l'
  <> metavar "LOG_LEVEL"
  <> value Info
  <> help "Plutip log-level, one of: trace, debug, info, warn, error")

logLevelFromString :: String -> LogLevel
logLevelFromString level
  | toLower level ==  "trace" = Trace
  | toLower level ==  "debug" = Debug
  | toLower level ==  "info" = Info
  | toLower level ==  "warn" = Warn
  | toLower level ==  "error" = Error
  | otherwise = Info

makePortParser :: String -> UInt.UInt -> Parser UInt.UInt
makePortParser name port = option (UInt.fromInt <$> int)
  (  long (name <> "-port")
  <> metavar "PORT"
  <> value port
  <> help ("The port " <> name <> " should listen on"))
