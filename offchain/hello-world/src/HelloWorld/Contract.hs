-- | Provides the hello world contract
module HelloWorld.Contract (
  InitHelloWorldSchema,
  IncHelloWorldSchema,
  ReadHelloWorldSchema,
  initialize,
  initializeHandler,
  increment,
  increment', -- needed for ContractModel tests
  read',
  read'', -- needed for ContractModel tests
) where

import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Data.Void (Void)

import Ledger (Datum (..), Redeemer (..))
import Ledger.Constraints (adjustUnbalancedTx, mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Tx (ChainIndexTxOut (..), TxOutRef (..), getCardanoTxId)
import Ledger.Value (AssetClass (unAssetClass), CurrencySymbol, TokenName(..), assetClass, singleton, valueOf)

import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  awaitPromise,
  awaitTxConfirmed,
  datumFromHash,
  endpoint,
  handleError,
  logError,
  logInfo,
  mapError,
  mkTxConstraints,
  ownPaymentPubKeyHash,
  submitUnbalancedTx,
  tell,
  utxosAt,
  HasEndpoint
 )
import Plutus.Contracts.Currency (CurrencyError, OneShotCurrency, currencySymbol, mintContract)
import PlutusTx (FromData, fromBuiltinData, toBuiltinData)
import PlutusTx.Builtins (mkI)

import HelloWorld.ValidatorProxy (helloValidator, helloValidatorAddress, helloValidatorHash)

-- | REST schema
type InitHelloWorldSchema = Endpoint "initialize" Integer
type IncHelloWorldSchema = Endpoint "increment" AssetClass 
type ReadHelloWorldSchema = Endpoint "read" AssetClass

initialize :: Contract (Last AssetClass) InitHelloWorldSchema Text ()
initialize = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"initialize" initializeHandler

initializeHandler :: Integer -> Contract (Last AssetClass) InitHelloWorldSchema Text ()
initializeHandler initialInt = do
  ownPkh <- ownPaymentPubKeyHash
  -- TODO: remove waitNSlots. this was added because the e2e tests are
  -- started immediately after the network is online. there seems to be a synchr problem
  cs <- currencySymbol <$> mapError (pack . show) (mintContract ownPkh [(TokenName "", 1)] :: Contract w s CurrencyError OneShotCurrency)
  let lookups = otherScript helloValidator
      tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI initialInt) (singleton cs "" 1)
  adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
  ledgerTx <- submitUnbalancedTx adjustedTx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell $ Last $ Just $ assetClass cs ""
  logInfo $ "Successfully initialized datum with value: " <> show initialInt


increment :: Contract () IncHelloWorldSchema Text ()
increment = increment'

increment' :: (HasEndpoint "increment" AssetClass s) => Contract () s Text ()
increment' = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"increment" incrementHandler

incrementHandler :: AssetClass -> Contract () s Text ()
incrementHandler ac = do
  let (cs, _) = unAssetClass ac
  maybeUTxO <- findUTxOWithToken cs
  case maybeUTxO of
    Nothing ->
      logInfo @Text "Couldn't find any UTxO at the script address for the given token"
    Just (txOutRef, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing ->
          logInfo @Text "No hello world datum found at the script address"
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

read' :: Contract (Last Integer) ReadHelloWorldSchema Text ()
read' = read''

read'' :: (HasEndpoint "read"AssetClass s) => Contract (Last Integer) s Text ()
read'' = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"read" readHandler

readHandler :: AssetClass -> Contract (Last Integer) s Text ()
readHandler ac = do
  let (cs, _) = unAssetClass ac
  maybeUtxo <- findUTxOWithToken cs
  case maybeUtxo of
    Nothing ->
      logInfo @Text "Couldn't find any UTxO at the script address for the given token"
    Just (txOutRef, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing -> do
          logInfo @Text "No hello world datum found at script address"
          tell $ Last Nothing
        (Just datum) -> do
          let lookups =
                unspentOutputs (Map.singleton txOutRef ciTxOut)
                  <> otherScript helloValidator
              tx =
                mustPayToOtherScript helloValidatorHash (Datum $ mkI datum) (singleton cs "" 1)
                  <> mustSpendScriptOutput txOutRef (Redeemer $ toBuiltinData ())
          adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
          ledgerTx <- submitUnbalancedTx adjustedTx
          awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo $ "Read datum value of " <> showText datum
          tell $ Last $ Just datum

showText :: Show a => a -> Text
showText = pack . show

findUTxOWithToken :: AsContractError e => CurrencySymbol -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut))
findUTxOWithToken cs = do
  singleElement . Map.toList . Map.filter (containsToken cs) <$> utxosAt helloValidatorAddress
  where
    containsToken :: CurrencySymbol -> ChainIndexTxOut -> Bool
    containsToken s chainIndexTxOut = valueOf (_ciTxOutValue chainIndexTxOut) s (TokenName "") == 1
    singleElement :: [a] -> Maybe a
    singleElement [x] = Just x
    singleElement _ = Nothing

getDatum' :: (FromData a, AsContractError e) => ChainIndexTxOut -> Contract w s e (Maybe a)
getDatum' (PublicKeyChainIndexTxOut _ _) = return Nothing
getDatum' (ScriptChainIndexTxOut _ _ eitherDatum _) =
  either
    (\datumHash -> (fromBuiltinData =<<) <$> getDatum <$$> datumFromHash datumHash) -- try to get the datum using the datumHash
    (return . fromBuiltinData . getDatum)
    eitherDatum
  where
    f <$$> x = (fmap . fmap) f x
