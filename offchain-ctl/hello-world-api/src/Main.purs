module Main
  ( main
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad
  ( Contract
  , ContractConfig(ContractConfig)
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , logInfo'
  , runContract_
  , traceContractConfig
  )
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash, applyArgsM)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , TransactionHash
  , TransactionOutput
  , TransactionInput(TransactionInput)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value as Value
import Contract.Wallet (mkNamiWalletAff)
import Data.BigInt as BigInt
import Data.Map as Map
import Effect.Aff (delay)
import ToData(class ToData,toData)
import Types.PlutusData (PlutusData(Constr,Integer))

data HelloRedemer = Inc | Spend

-- TODO this should probably be generics, but
-- I couldn't get generics to work
instance ToData HelloRedemer where
  toData Inc = Constr zero []
  toData Spend = Constr one []

incRedeemer :: Redeemer
incRedeemer = Redeemer (toData Inc)

spendRedeemer :: Redeemer
spendRedeemer = Redeemer (toData Spend)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.Hello"
    mvalidator <- helloScript 4
    case mvalidator of
      Nothing -> logInfo' "applyArgsM failed"
      Just validator -> do
        vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
        logInfo' "Attempt to lock value"

        txId <- payToHello 5 vhash
        logInfo' $ "Woooo! You did a CTL transaction. Id: " <> show txId

        -- There's gotta be a way to do this with MaybeT Eff or something
        -- but I'm not sure yet
        -- It's liftContractM
        maybeTxin <- waitForTx 60 vhash txId

        case maybeTxin of
          Just txin -> do
            logInfo' "Try to increment"
            -- this failed with 10 like you would want
            txId2 <- incHello 9 txin vhash validator

            maybeTxIn2 <- waitForTx 60 vhash txId2
            case maybeTxIn2 of
              Just txIn2 -> do
                logInfo' "Try to spend"
                txId3 <- spendFromHello txIn2 vhash validator
                logInfo' $ "finished with txid:" <> show txId3
              Nothing -> logInfo' "failed"
          Nothing -> logInfo' "failed"

-- TODO it would probably be better to make this return the txInput
waitForTx :: Int -> ValidatorHash -> TransactionHash -> Contract () (Maybe TransactionInput)
waitForTx n vhash txid = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array (TransactionInput /\ TransactionOutput)) of
      Nothing ->
        if (n <= 0)
          then do
            pure Nothing
          else do
            logInfo' $ "No tx yet, waiting for: " <> show n <> " more seconds"
            (liftAff <<< delay <<< wrap) 1000.0
            waitForTx (n - 1) vhash txid
      Just txin -> do
        logInfo' $ "found tx:" <> show txid
        pure $ Just txin
  where
    hasTransactionId :: forall a. TransactionInput /\ a -> Boolean
    hasTransactionId (TransactionInput tx /\ _) =
      tx.transactionId == txid

payToHello :: Int -> ValidatorHash -> Contract () TransactionHash
payToHello n vhash = do
  let
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash (Datum $ toData (BigInt.fromInt n))
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints

incHello
  :: Int
  -> TransactionInput
  -> ValidatorHash
  -> Validator
  -> Contract () TransactionHash
incHello n txInput vhash validator = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
        <> Lookups.unspentOutputs utxos
    constraints :: TxConstraints Unit Unit
    constraints =
      (Constraints.mustSpendScriptOutput txInput incRedeemer)
      <>
      (Constraints.mustPayToScript vhash (Datum $ toData (BigInt.fromInt n :: BigInt.BigInt))
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000
      )

  buildBalanceSignAndSubmitTx lookups constraints

spendFromHello
  :: TransactionInput
  -> ValidatorHash
  -> Validator
  -> Contract () TransactionHash
spendFromHello txInput vhash validator = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput spendRedeemer
  buildBalanceSignAndSubmitTx lookups constraints

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  BalancedSignedTransaction bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx.signedTxCbor
  logInfo' $ "Tx ID: " <> show txId
  pure txId

helloScript :: Int -> Contract () (Maybe Validator)
helloScript n =
  let maybeParamValidator =
        map wrap $ hush $ decodeAeson $ fromString
          "5901a4010000323232323232323232323222223233322232323253330103370e90000010991919191919299980b19b87480080084c8c8c94ccc064cdc3a400000426666446666030466e3c00cdd7180f98100008009111801191811181098118021bad3021302030220031225001375c60380026eb0c070c8c070c070c070c070c058004c06c01c8c94ccc06ccdc399b80011012001149858dd68008b0b180e801180e8009baa32301a3013301b0013253330160011613253330170011301b0021630190013332223333016301a0010032332301922533301c00114bd70099299980f1802000899aba0001300330200021300330200023020001301b32301e301f001301d301c301e001003163758603200a6eb0c064010c06400458c068008c068004dd5180b180a801980a000980a980a000980a0020a4c602800460280026ea8008dd68020018011bad00423007300700130012225333005001122500115333006300230090011223002300b00313300300230080012323002233002002001230022330020020015573eae6888cdd780118021802800aba25742460046ea800555cf2ab9d01"
   in case maybeParamValidator of
          Nothing -> pure Nothing
          Just paramValidator ->
            applyArgsM
              paramValidator
              [Integer $ BigInt.fromInt n]
              -- TODO It'd be cool if this could be an Integer not Data

