module Ctl.Utils.Test
  ( runWithMode
  , useRunnerSimple
  , expectScriptError
  , runTwoWallets
  , getPlutipConfig
  -- Types
  , EnvSpec
  , EnvRunner
  ) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, ContractEnv, withContractEnv)
import Contract.Test.Plutip (PlutipConfig, runContractInEnv, withKeyWallet, withPlutipContractEnv)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (class MonadError)
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Data.Array (replicate)
import Data.BigInt as BigInt
import Data.Identity (Identity)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.String (Pattern(Pattern), contains, trim)
import Data.Time.Duration (Minutes(..), fromDuration)
import Data.UInt as UInt
import Data.Unfoldable (replicateA)
import Effect.Aff.Retry (limitRetries, limitRetriesByCumulativeDelay, recovering)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message, throw)
import Effect.Random (randomInt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, exists, unlink)
import Node.Process (lookupEnv)
import Test.Spec (SpecT, before, sequential)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Ctl.Utils.Test.Types (Mode(..))

type EnvRunner = (ContractEnv () -> KeyWallet -> Aff Unit) -> Aff Unit
type EnvSpec = SpecT Aff EnvRunner Identity Unit

-- | Given the execution Mode and an EnvSpec
-- runs the spec in that mode
runWithMode :: Mode -> EnvSpec -> Aff Unit
runWithMode mode spec = do
  runnerGetter <- getEnvRunner mode
  runSpec'
    defaultConfig
      { timeout = Just $ fromDuration $ Minutes 10.0 }
    [ specReporter ]
    $ before runnerGetter
    $ sequential
    $ spec

-- | Prepares a contract to be run as an EnvSpec
-- tye return type should be thought of as (EnvRunner -> Aff Unit)
-- the function `it` transforms this type into an EnvSpec
useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  retryOkayErrs $ runner \env alice ->
    runContractInEnv env
      $ withKeyWallet alice
      $ void contract

runTwoWallets :: forall a. (KeyWallet -> KeyWallet -> Contract () a) -> EnvRunner -> Aff Unit
runTwoWallets contract _ = retryOkayErrs $ do
  cfg <- getPlutipConfig
  withPlutipContractEnv cfg (defaultWallet /\ defaultWallet) $
    \env (alice /\ bob) ->
      runContractInEnv env
        $ void
        $ contract alice bob

retryOkayErrs :: Aff Unit -> Aff Unit
retryOkayErrs aff =
  recovering
    (limitRetriesByCumulativeDelay (Minutes 10.0) $ limitRetries 5)
    [ \status err' -> do
        log $ "retrying with" <> show status
        let err = trim $ message err'
        if err `elem` badErrors then pure false
        else do
          if err `elem` (trim <$> okayErrs) then pure true
          else do
            log $ "failed with an error not makred as retryable"
            log $ "if this error is okay add it to the okayErrs list in ./test/TestUtil.purs"
            log $ "exact error was:" <> show err
            log $ "\nfull error:\n" <> show err' <> "\n"
            pure $ false
    ]
    \_ -> aff

-- Errors where we don't
-- want to suggest adding
-- them to okayErrs
badErrors :: Array String
badErrors =
  [ "Expected Error"
  ]

okayErrs :: Array String
okayErrs =
  [ "(ClientHttpError There was a problem making the request: request failed)"
  , "Process ogmios-datum-cache exited. Output:"
  , "Process ctl-server exited. Output:"
  , "timed out waiting for tx"
  , "Error: Command failed: psql -h 127.0.0.1"
  , "Error: Unable to run the following services, because the ports are occupied:"
  , "[object Object]"
  ]

-- returns a contiunation that gets the EnvRunner
-- This is nesecary to allow control over which parts
-- run once vs each time
getEnvRunner :: Mode -> Aff (Aff EnvRunner)
getEnvRunner Local = do
  oldLogsExist <- exists "apiTest.log"
  when oldLogsExist $ unlink "apiTest.log"
  pure $ do
    newCfg <- getPlutipConfig
    pure $ withPlutipContractEnv newCfg $ defaultWallet
getEnvRunner Testnet = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  key <- privatePaymentKeyFromFile $ testResourcesDir <> "/wallet.skey"
  stakeKey <- privateStakeKeyFromFile $ testResourcesDir <> "/staking.skey"
  let keyWallet = privateKeysToKeyWallet key (Just stakeKey)
  pure $ pure
    $ \f -> withContractEnv (testnetConfig { logLevel = Warn }) $ \env -> f (env :: ContractEnv ()) (keyWallet :: KeyWallet)

defaultWallet :: Array BigInt.BigInt
defaultWallet = replicate 5 $ BigInt.fromInt 40_000_000

-- For "fails when seedTx is not spent it's important that
-- there are other utxos with enough money to cover fees
-- so the seedTx doesn't have to be spent

getFreePort :: Aff Int
getFreePort = do
  randomPort <- liftEffect $ randomInt 1024 0xFFFF
  isGood <- isPortAvailable (UInt.fromInt randomPort)
  if isGood then pure randomPort
  else getFreePort

-- Finds 5 free ports and returns a plutip config using those ports
getPlutipConfig :: Aff PlutipConfig
getPlutipConfig = do
  replicateA 5 getFreePort >>= case _ of
    [ p1, p2, p3, p4, p5 ] -> pure $
      { host: "127.0.0.1"
      , port: UInt.fromInt p1
      , logLevel: Warn
      -- Server configs are used to deploy the corresponding services.
      , ogmiosConfig:
          { port: UInt.fromInt p2
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , ogmiosDatumCacheConfig:
          { port: UInt.fromInt p3
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , ctlServerConfig: Just
          { port: UInt.fromInt p4
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , postgresConfig:
          { host: "127.0.0.1"
          , port: UInt.fromInt p5
          , user: "ctxlib"
          , password: "ctxlib"
          , dbname: "ctxlib"
          }
      , customLogger: Just (ourLogger "apiTest.log")
      , suppressLogs: false
      , hooks: undefined
      }
    _ -> liftEffect $ throw "replicateM returned list of the wrong length in plutipConfig"

ourLogger :: String -> LogLevel -> Message -> Aff Unit
ourLogger path level msg = do
  pretty <- prettyFormatter msg
  when (msg.level >= level) $ log pretty
  appendTextFile UTF8 path ("\n" <> pretty)

expectScriptError
  :: forall m t
   . MonadError Error m
  => MonadEffect m
  => m t
  -> m Unit
expectScriptError =
  expectErrorPred (\err -> contains (Pattern "Script failure") (message err))

expectErrorPred
  :: forall m t
   . MonadError Error m
  => MonadEffect m
  => (Error -> Boolean)
  -> m t
  -> m Unit
expectErrorPred pred a =
  try a >>= case _ of
    Left err -> when (not $ pred err) $ do
      liftEffect $ log $ "Threw an error but it didn't match predicate"
      throwError err
    Right _ -> throwError $ error "Expected error"
