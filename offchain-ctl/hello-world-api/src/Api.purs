module Api
  (sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  ,helloScript
  ,enoughForFees
  ) where

import Contract.Prelude

import CBOR as CBOR
import Util(buildBalanceSignAndSubmitTx,waitForTx,getUtxos)

import Control.Bind (bindFlipped)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration(Minutes(..))
import Type.Proxy (Proxy(Proxy))

import Contract.Address (Address)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Monad ( Contract , liftContractM , logInfo', liftQueryM, liftedE, liftedM)
import Contract.PlutusData (Datum(Datum),Redeemer(Redeemer), FromData, ToData, toData, getDatumByHash, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, applyArgsM)
import Contract.Transaction ( TransactionInput, TransactionOutput, mkUnbalancedTx, balanceAndSignTx, submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt, UtxoM)
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

{-
increment :: CurrencySymbol -> Contract () IncHelloWorldSchema Text ()
increment cs = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"increment" $ const $ incrementHandler cs


getDatum' :: (FromData a, AsContractError e) => ChainIndexTxOut -> Contract w s e (Maybe a)
getDatum' (PublicKeyChainIndexTxOut _ _) = return Nothing
getDatum' (ScriptChainIndexTxOut _ _ eitherDatum _) =
  either
    (\datumHash -> (fromBuiltinData =<<) <$> getDatum <$$> datumFromHash datumHash) -- try to get the datum using the datumHash
    (return . fromBuiltinData . getDatum)
    eitherDatum
  where
    f <$$> x = (fmap . fmap) f x


incrementHandler :: AsContractError e => CurrencySymbol -> Contract w s e ()
incrementHandler cs = do
  maybeUTxO <- findUTxOWithToken cs
  case maybeUTxO of
    Nothing ->
      logInfo @Text "Couldn't find any UTxO at the script address for the given token"
    Just (txOutRef, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing ->
          logInfo @Text $ "No hello world datum found at the script address"
        Just oldDatum -> do
          let updatedHelloWorldDatum = oldDatum + 1
              lookups =
                unspentOutputs (Map.singleton txOutRef ciTxOut)
                  <> otherScript helloValidator
              tx =
                mustPayToOtherScript helloValidatorHash (Datum $ mkI updatedHelloWorldDatum) (singleton cs "" 1)
                  <> mustSpendScriptOutput txOutRef (Redeemer $ toBuiltinData ())
          adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
          ledgerTx <- submitUnbalancedTx adjustedTx
          awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo $ "Successfully incremented to value " <> showText updatedHelloWorldDatum

-}
incrementHandler :: forall (r :: Row Type). Validator -> ValidatorHash -> CurrencySymbol -> Contract r Unit
incrementHandler helloVal helloHash cs = do
  let helloAdr = scriptHashAddress helloHash
  maybeUtxo <- findUtxoWithToken helloAdr cs
  case maybeUtxo of
    Nothing -> pure unit -- lookup how to log an error message
    Just tup -> do
      let txin  = get1 tup
          txout = get2 tup
      maybeHelloWorldDatum <- getDatum'' (Proxy :: Proxy BigInt.BigInt) txout
      case maybeHelloWorldDatum of
        Nothing -> pure unit -- log error here
        Just oldDatum -> do
          let newDatum = oldDatum + 1
              newHelloWorldDatum = Datum (toData newDatum)
              unspentMap = Map.fromFoldable maybeUtxo
              lookups = 
                unspentOutputs unspentMap
                  <> validator helloVal
              outputVal = Value.mkSingletonValue' cs Value.adaToken (BigInt.fromInt 1)
              tx =
                mustPayToScript helloHash newHelloWorldDatum outputVal
                  <> mustSpendScriptOutput txin unitRedeemer
          unbalancedTx <- liftedE $ wrap $ liftQueryM $ mkUnbalancedTx lookups tx
          balancedTx <- liftedM "Could not balance Transaction." $ balanceAndSignTx unbalancedTx
          txHash <- submit balancedTx
          logInfo' ("Tx hash: " <> show txHash)

-- | Same as PAB version, except you can 
-- | change the Address value.
findUtxoWithToken :: forall (r :: Row Type). Address -> CurrencySymbol -> Contract r (Maybe (TransactionInput /\ TransactionOutput))
findUtxoWithToken adrs cs = do
  -- Have to use map since the UtxoM is in two
  -- layers of Functors.
  bindFlipped singleElement <<< map Map.toUnfoldable <<< map (Map.filter (containsToken cs)) <<< map unwrap <$> utxosAt adrs
  where 
    containsToken :: CurrencySymbol -> TransactionOutput -> Boolean
    containsToken csymb txout = (BigInt.fromInt 1) == Value.valueOf ((_.amount) (unwrap txout)) csymb Value.adaToken
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
getDatum'' prox = getDatum'
