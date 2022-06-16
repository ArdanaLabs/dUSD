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
import Contract.PlutusData (unitRedeemer, Datum(Datum),Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
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
import Types.PlutusData (PlutusData(Constr))

data HelloRedemer = Inc | Spend

-- TODO this should probably be generics, but
-- I couldn't get generics to work
instance ToData HelloRedemer where
  toData Inc = Constr zero []
  toData Spend = Constr one []

spendRedeemer :: Redeemer
spendRedeemer = Redeemer (toData Spend)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.Hello"
    validator <- liftContractM "Invalid script JSON" $ helloScript
    vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
    logInfo' "Attempt to lock value"

    txId <- payToHello 5 vhash
    logInfo' $ "Woooo! You did a CTL transaction. Id: " <> show txId

    -- There's gotta be a way to do this with MaybeT Eff or something
    -- but I'm not sure yet
    maybeTxin <- waitForTx 60 vhash txId

    case maybeTxin of
      Just txin -> do
        logInfo' "Try to increment"
        txId2 <- incHello 6 txin vhash validator

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
      (Constraints.mustSpendScriptOutput txInput unitRedeemer)
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

helloScript :: Maybe Validator
helloScript = map wrap $ hush $ decodeAeson $ fromString
  "59019f0100003232323232323232323232222333222323232533300e3370e90000010991919191919299980a19b87480080084c8c8c94ccc05ccdc3a40040042c2666644666602c466e3c00cdd7180e980f0008009111801191810180f98108021bad301f301e30200031225001375c60340026eb0c068c8c068c068c068c068c050004c06401c8c94ccc064cdc399b800114800800452616375a0022c603600460360026ea8c8c060c044c064004c94ccc050004584c94ccc0540044c06400858c05c004ccc888cccc050c06000400c8cc8c05c894ccc06800452f5c0264a6660386008002266ae80004c00cc0780084c00cc078008c078004c064c8c070c074004c06cc068c07000400c58dd6180b8029bac3017004301700116301800230180013754602860260066024002602660240026024008293180900118090009baa002375a0060040024600e600e0026002444a66600a002244a0022a66600c60046012002244600460160062660060046010002464600446600400400246004466004004002aae7d5cd1119baf002300430050015744ae848c008dd5000aab9e5573b"
