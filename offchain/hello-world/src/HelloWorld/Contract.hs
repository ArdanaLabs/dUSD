-- | Provides the hello world contract
module HelloWorld.Contract (
  InitHelloWorldSchema,
  IncHelloWorldSchema,
  ReadHelloWorldSchema,
  ReleaseHelloWorldSchema,
  initialize,
  initializeHandler,
  increment,
  increment', -- needed for ContractModel tests
  read',
  read'', -- needed for ContractModel tests
  release,
  release', -- needed for ContractModel tests
) where

import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Data.Void (Void)

import Ledger (Datum (..), Redeemer (..))
import Ledger.Constraints (adjustUnbalancedTx, mustPayToOtherScript, mustPayToPubKey, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Constraints qualified as Constraint
import Ledger.Tx (ChainIndexTxOut (..), TxOutRef (..))
import Ledger.Value (AssetClass, TokenName (..), assetClass, assetClassValue, assetClassValueOf, singleton)

import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  HasEndpoint,
  awaitPromise,
  datumFromHash,
  endpoint,
  handleError,
  logError,
  logInfo,
  mapError,
  mkTxConstraints,
  ownPaymentPubKeyHash,
  submitTxConfirmed,
  tell,
  utxosAt,
 )
import Plutus.Contracts.Currency (CurrencyError, OneShotCurrency, currencySymbol, mintContract)
import PlutusTx (FromData, fromBuiltinData, toBuiltinData)
import PlutusTx.Builtins (mkI)

import HelloWorld.ValidatorProxy (HelloRedeemer (..), helloValidator, helloValidatorAddress, helloValidatorHash)

-- | REST schema
type InitHelloWorldSchema = Endpoint "initialize" Integer

type IncHelloWorldSchema = Endpoint "increment" AssetClass
type ReadHelloWorldSchema = Endpoint "read" AssetClass
type ReleaseHelloWorldSchema = Endpoint "release" AssetClass

initialize :: Contract (Last AssetClass) InitHelloWorldSchema Text ()
initialize = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"initialize" initializeHandler

initializeHandler :: Integer -> Contract (Last AssetClass) InitHelloWorldSchema Text ()
initializeHandler initialInt = do
  ownPkh <- ownPaymentPubKeyHash
  cs <- currencySymbol <$> mapError (pack . show) (mintContract ownPkh [(TokenName "", 1)] :: Contract w s CurrencyError OneShotCurrency)
  let lookups = otherScript helloValidator
      tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI initialInt) (singleton cs "" 1)
  mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
  tell $ Last $ Just $ assetClass cs ""
  logInfo $ "Successfully initialized datum with value: " <> show initialInt

increment :: Contract () IncHelloWorldSchema Text ()
increment = increment'

increment' :: (HasEndpoint "increment" AssetClass s) => Contract () s Text ()
increment' = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"increment" incrementHandler

incrementHandler :: AssetClass -> Contract () s Text ()
incrementHandler ac = do
  maybeUTxO <- findUTxOWithToken ac
  case maybeUTxO of
    Nothing ->
      logError @Text "Couldn't find any UTxO at the script address for the given token"
    Just (txOutRef, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing ->
          logError @Text "No hello world datum found at the script address"
        Just oldDatum -> do
          let updatedHelloWorldDatum = oldDatum + 1
              lookups =
                unspentOutputs (Map.singleton txOutRef ciTxOut)
                  <> otherScript helloValidator
              tx =
                mustPayToOtherScript helloValidatorHash (Datum $ mkI updatedHelloWorldDatum) (assetClassValue ac 1)
                  <> mustSpendScriptOutput txOutRef (Redeemer $ toBuiltinData Increment)
          mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
          logInfo $ "Successfully incremented to value " <> showText updatedHelloWorldDatum

read' :: Contract (Last Integer) ReadHelloWorldSchema Text ()
read' = read''

read'' :: (HasEndpoint "read" AssetClass s) => Contract (Last Integer) s Text ()
read'' = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"read" readHandler

readHandler :: AssetClass -> Contract (Last Integer) s Text ()
readHandler ac = do
  maybeUtxo <- findUTxOWithToken ac
  case maybeUtxo of
    Nothing ->
      logError @Text "Couldn't find any UTxO at the script address for the given token"
    Just (_, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing -> do
          logError @Text "No hello world datum found at script address"
          tell $ Last Nothing
        (Just datum) -> do
          logInfo $ "Read datum value of " <> showText datum
          tell $ Last $ Just datum

release :: Contract () ReleaseHelloWorldSchema Text ()
release = release'

release' :: (HasEndpoint "release" AssetClass s) => Contract () s Text ()
release' = forever $ handleError logError $ awaitPromise $ endpoint @"release" releaseHandler

-- | Release the HelloWorld UTxO back to a public key.
releaseHandler :: AssetClass -> Contract w s Text ()
releaseHandler ac = do
  ownPkh <- ownPaymentPubKeyHash
  maybeUTxO <- findUTxOWithToken ac
  maybe
    (logError @Text "Couldn't find any UTxO at the script address for the given token")
    ( \(txOutRef, ciTxOut) -> do
        let value = assetClassValue ac 1
            lookups =
              otherScript helloValidator
                <> unspentOutputs (Map.singleton txOutRef ciTxOut)
                <> Constraint.ownPaymentPubKeyHash ownPkh
            tx =
              mustSpendScriptOutput txOutRef (Redeemer $ toBuiltinData Release)
                <> mustPayToPubKey ownPkh value
        mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
        logInfo @Text "Successfully released the scripts HelloWorld UTxO"
    )
    maybeUTxO

showText :: Show a => a -> Text
showText = pack . show

findUTxOWithToken :: AsContractError e => AssetClass -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut))
findUTxOWithToken ac = do
  singleElement . Map.toList . Map.filter (containsToken ac) <$> utxosAt helloValidatorAddress

containsToken :: AssetClass -> ChainIndexTxOut -> Bool
containsToken ac chainIndexTxOut = assetClassValueOf (_ciTxOutValue chainIndexTxOut) ac == 1

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
