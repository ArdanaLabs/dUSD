module Api
  (payToHello
  ,incHello
  ,redeemFromHello
  ,helloScript
  ) where

import Contract.Prelude

import CBOR as CBOR
import Util(buildBalanceSignAndSubmitTx,waitForTx,getUtxos)

import Data.BigInt as BigInt

import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad ( Contract , liftContractM , logInfo')
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgsM)
import Contract.Transaction ( TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import ToData(class ToData,toData)
import Types.PlutusData (PlutusData(Constr,Integer))

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
  utxos <- getUtxos vhash
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
  utxos <- getUtxos vhash
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos
    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput spendRedeemer
  _ <- buildBalanceSignAndSubmitTx lookups constraints
  logInfo' "finished"

helloScript :: Int -> Contract () Validator
helloScript n = do
  let maybeParamValidator :: Maybe Validator
      maybeParamValidator =
          CBOR.hello
            # fromString
            # decodeAeson
            # hush
            # map wrap
  paramValidator <- liftContractM "decoding failed" maybeParamValidator
  liftContractM "apply args failed" =<< applyArgsM paramValidator [Integer $ BigInt.fromInt n]
         -- TODO It'd be cool if this could be an Integer not Data

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

