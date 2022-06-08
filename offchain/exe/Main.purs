module Main
  ( main
  ) where

import Contract.Prelude
import Contract.Address (NetworkId(TestnetId))
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad (ConfigParams(..), LogLevel(Trace), defaultDatumCacheWsConfig, defaultOgmiosWsConfig, defaultServerConfig, defaultSlotConfig, launchAff_, liftContractM, liftedE, liftedM, logInfo', mkContractConfig, runContract_)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (BalancedSignedTransaction(BalancedSignedTransaction), balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (Wallet, mkNamiWalletAff)
import Data.BigInt as BigInt


alwaysSucceedsScriptCbor :: String
alwaysSucceedsScriptCbor = "4d01000033222220051200120011"

walletToConfig :: Maybe Wallet -> ConfigParams ()
walletToConfig wallet =
  ConfigParams
    -- The server defaults below are also exported from
    -- `Contract.Monad`
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Trace
    , extraConfig: {}
    , slotConfig: defaultSlotConfig
    , wallet
    }


alwaysSucceedsScript :: Maybe Validator
alwaysSucceedsScript =
  map wrap $ hush $ decodeAeson
    $ fromString
        alwaysSucceedsScriptCbor


main :: Effect Unit
main =
  launchAff_
    $ do
        log "Started"
        wallet <- Just <$> mkNamiWalletAff
        cfg <-
          mkContractConfig
            $ walletToConfig wallet
        runContract_ cfg
          $ do
              logInfo' "Running Examples.AlwaysSucceeds"
              validator <- liftContractM "Invalid script JSON" $ alwaysSucceedsScript
              vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
              logInfo' $ show vhash
              let
                -- Note that CTL does not have explicit equivalents of Plutus'
                -- `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
                -- of a "current" script. Thus, we have the single constraint
                -- `mustPayToScript`, and all scripts must be explicitly provided to build
                -- the transaction (see the value for `lookups` below as well)
                constraints :: Constraints.TxConstraints Unit Unit
                constraints =
                  Constraints.mustPayToScript vhash unitDatum
                    $ Value.lovelaceValueOf
                    $ BigInt.fromInt 2_000_000

                lookups :: Lookups.ScriptLookups PlutusData
                lookups = Lookups.validator validator
              logInfo' $ show lookups
              ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
              BalancedSignedTransaction bsTx <-
                liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
              txId <- submit bsTx.signedTxCbor
              logInfo' $ "Tx ID: " <> show txId
