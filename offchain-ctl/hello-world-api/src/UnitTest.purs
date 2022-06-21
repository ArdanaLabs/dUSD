module UnitTest
  (helloUnitTest
  ) where

import Contract.Prelude

import CBOR as CBOR
import Util(buildBalanceSignAndSubmitTx,waitForTx)

import Data.BigInt as BigInt
import Data.Map as Map

import Contract.Address (scriptHashAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad ( Contract , liftContractM , logInfo')
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgsM, validatorHash)
import Contract.Transaction ( TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value as Value
import ToData(class ToData,toData)
import Types.PlutusData (PlutusData(Constr,Integer))

helloUnitTest :: Contract () Unit
helloUnitTest = do
  logInfo' "Running Examples.Hello"
  validator <- helloScript 4
  vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
  payToHello 5 vhash >>= incHello 9 vhash validator >>= redeemFromHello vhash validator

payToHello :: Int -> ValidatorHash -> Contract () TransactionInput
payToHello n vhash = do
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript
        vhash
        (Datum $ n
          # BigInt.fromInt
          # toData
        )
        enoughForFees
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "gave up waiting for payToHello TX" =<< waitForTx 60 vhash txId

incHello
  :: Int
  -> ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () TransactionInput
incHello n vhash validator txInput = do
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
      (Constraints.mustPayToScript
        vhash
        (Datum $ n
          # BigInt.fromInt
          # toData
        )
        enoughForFees
      )
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  liftContractM "failed waiting for increment" =<< waitForTx 60 vhash txId

redeemFromHello
  :: ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () Unit
redeemFromHello vhash validator txInput = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput spendRedeemer
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

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

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 2_000_000

helloScript :: Int -> Contract () Validator
helloScript n =
  let maybeParamValidator :: Maybe Validator
      maybeParamValidator =
        CBOR.hello
          # fromString
          # decodeAeson
          # hush
          # map wrap
   in do
      paramValidator <- liftContractM "decoding failed" maybeParamValidator
      liftContractM "apply args failed" =<< applyArgsM paramValidator [Integer $ BigInt.fromInt n]
         -- TODO It'd be cool if this could be an Integer not Data

