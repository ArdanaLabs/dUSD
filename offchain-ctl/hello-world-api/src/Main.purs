module Main
  ( main
  ) where


import Contract.Prelude

import CBOR as CBOR

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
main =
  launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    logInfo' "Running Examples.Hello"
    validator <- helloScript 4
    vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator

    -- Pay to hello
    logInfo' "Starting pay to hello"
    txId <- payToHello 5 vhash
    txIn1 <- liftContractM "gave up waiting for payToHello TX" =<< waitForTx 60 vhash txId

    -- Increment
    logInfo' "Starting increment"
    txId2 <- incHello 9 txIn1 vhash validator -- this failed with 10 like you would want
    txIn2 <- liftContractM "failed waiting for increment" =<< waitForTx 60 vhash txId2

    -- Redeem
    logInfo' "Starting redeem ad"
    _txId3 <- redeemFromHello txIn2 vhash validator

    logInfo' "finished"



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
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash (Datum $ toData (BigInt.fromInt n))
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000


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

redeemFromHello
  :: TransactionInput
  -> ValidatorHash
  -> Validator
  -> Contract () TransactionHash
redeemFromHello txInput vhash validator = do
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

helloScript :: Int -> Contract () Validator
helloScript n =
  let maybeParamValidator :: Maybe Validator
      maybeParamValidator =
        map wrap $ hush $ decodeAeson $ fromString CBOR.hello
   in do
      paramValidator <- liftContractM "decoding failed" maybeParamValidator
      liftContractM "apply args failed" =<< applyArgsM paramValidator [Integer $ BigInt.fromInt n]
         -- TODO It'd be cool if this could be an Integer not Data

