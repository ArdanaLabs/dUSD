module Api
  (sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  ,helloScript
  ,enoughForFees
  ,incrementHandler'
  ) where

import Contract.Prelude

import CBOR as CBOR
import Util(buildBalanceSignAndSubmitTx,waitForTx,getUtxos)

import Control.Bind (bindFlipped)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration(Minutes(..))
import Type.Proxy (Proxy(Proxy))

import Contract.Address (Address, scriptHashAddress)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad ( Contract , liftContractM , logInfo', liftQueryM, liftedE, liftedM, liftContractAffM)
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer), class FromData, fromData, getDatumByHash, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgsM, validatorHash)
import Contract.Transaction ( TransactionInput, TransactionOutput, mkUnbalancedTx, balanceAndSignTx, submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Value (CurrencySymbol, TokenName, Value)
import ToData(class ToData,toData)
import Types.PlutusData (PlutusData(Constr,Integer))

sendDatumToScript :: Int -> ValidatorHash -> Contract () TransactionInput
sendDatumToScript n vhash = do
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
  liftContractM "gave up waiting for sendDatumToScript TX" =<< waitForTx (Minutes 1.0) vhash txId

setDatumAtScript
  :: Int
  -> ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () TransactionInput
setDatumAtScript n vhash validator txInput = do
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
  liftContractM "failed waiting for increment" =<< waitForTx (Minutes 1.0) vhash txId

redeemFromScript
  :: ValidatorHash
  -> Validator
  -> TransactionInput
  -> Contract () Unit
redeemFromScript vhash validator txInput = do
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
          CBOR.paramHello
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
  toData Inc = Constr (BigInt.fromInt 0) []
  toData Spend = Constr (BigInt.fromInt 1) []

incRedeemer :: Redeemer
incRedeemer = Redeemer (toData Inc)

spendRedeemer :: Redeemer
spendRedeemer = Redeemer (toData Spend)

enoughForFees :: Value.Value
enoughForFees = Value.lovelaceValueOf $ BigInt.fromInt 6_000_000

-- | Like `incrementHandler`, but derives the
-- | `ValidatorHash` automatically.`
incrementHandler' :: forall (r :: Row Type). Validator -> CurrencySymbol -> Contract r Unit 
incrementHandler' val cs = do
  valHash <- liftContractAffM "Couldn't hash validator script." (validatorHash val)
  incrementHandler val valHash cs

-- | The main contract for increment.
incrementHandler :: forall (r :: Row Type). Validator -> ValidatorHash -> CurrencySymbol -> Contract r Unit
incrementHandler helloVal helloHash cs = do
  let helloAdr = scriptHashAddress helloHash
  maybeUtxo <- findUtxoWithToken helloAdr cs
  (Tuple txin txout) <- liftContractM "Couldn't find any UTxO at script address for given token." maybeUtxo
  maybeHelloWorldDatum <- getDatum'' (Proxy :: Proxy BigInt.BigInt) txout
  oldDatum <- liftContractM "No hello world datum found at script address." maybeHelloWorldDatum
  let newDatum = oldDatum + (BigInt.fromInt 1)
      newHelloWorldDatum = Datum (toData newDatum)
      unspentMap = Map.fromFoldable maybeUtxo
      lookups :: Lookups.ScriptLookups PlutusData
      lookups = 
        Lookups.unspentOutputs unspentMap
          <> Lookups.validator helloVal
      outputVal = Value.singleton cs Value.adaToken (BigInt.fromInt 1)
      tx :: Constraints.TxConstraints Unit Unit
      tx =
        Constraints.mustPayToScript helloHash newHelloWorldDatum outputVal
          <> Constraints.mustSpendScriptOutput txin unitRedeemer
  unbalancedTx <- liftedE $ wrap $ liftQueryM $ mkUnbalancedTx lookups tx
  balancedTx <- liftedM "Could not balance Transaction." $ balanceAndSignTx unbalancedTx
  txHash <- submit balancedTx
  logInfo' ("Tx hash: " <> show txHash)
  txi <- liftContractM "Gave up waiting for increment Tx." =<< waitForTx (Minutes 1.0) helloHash txHash
  logInfo' ("TxInput: " <> show txi)

-- | Same as PAB version, except you can 
-- | change the Address value.
findUtxoWithToken :: forall (r :: Row Type). Address -> CurrencySymbol -> Contract r (Maybe (Tuple TransactionInput TransactionOutput))
findUtxoWithToken adrs cs = do
  -- Have to use map since the UtxoM is in two
  -- layers of Functors.
  bindFlipped singleElement <<< map Map.toUnfoldable <<< map (Map.filter (containsToken cs)) <<< map unwrap <$> utxosAt adrs
  where 
    containsToken :: CurrencySymbol -> TransactionOutput -> Boolean
    containsToken csymb txout = (BigInt.fromInt 1) == Value.valueOf (txout #% _.amount) csymb Value.adaToken
    singleElement :: forall (a :: Type). Array a -> Maybe a
    singleElement [x] = Just x
    singleElement  _  = Nothing

-- | Get the datum from a `TransactionOutput`.
getDatum' :: forall (a :: Type) (r :: Row Type). (FromData a) => TransactionOutput -> Contract r (Maybe a)
getDatum' txout' = do
  datm <- maybe (pure Nothing) getDatumByHash txout.dataHash
  let datb = unwrap <$> datm -- to get the raw bytes
  pure (datb >>= fromData)
  where
    txout = unwrap txout'

-- | A version of `getDatum'` with a proxy.
getDatum'' :: forall (a :: Type) (r :: Row Type). (FromData a) => Proxy a -> TransactionOutput -> Contract r (Maybe a)
getDatum'' _prox = getDatum'


-- | Reverse function application on 
-- | a wrapped newtype.
rapplyUnwrap :: forall (new :: Type) (old :: Type) (a :: Type). (Newtype new old) => new -> (old -> a) -> a
rapplyUnwrap val f = f (unwrap val)

infixl 1 rapplyUnwrap as #%
