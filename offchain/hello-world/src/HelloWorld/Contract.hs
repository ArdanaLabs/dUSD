-- | Provides the hello world contract
module HelloWorld.Contract (
  InitHelloWorldSchema,
  IncHelloWorldSchema,
  ReadHelloWorldSchema,
  initialize,
  initializeHandler,
  increment,
  read',
) where

import Control.Monad (forever)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Data.Void (Void)

import Ledger (Datum (..), Redeemer (..), pubKeyHashAddress)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs, mintingPolicy, mustMintValue, mustSpendPubKeyOutput)
import Ledger.Tx (ChainIndexTxOut (..), TxOutRef (..), getCardanoTxId)
import Ledger.Value (CurrencySymbol, TokenName (..), singleton, valueOf)

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
  mkTxConstraints,
  ownPaymentPubKeyHash,
  submitUnbalancedTx,
  tell,
  utxosAt,
  submitTxConfirmed
 )
import Plutus.MintingPolicies.NFT
import PlutusTx (FromData, fromBuiltinData, toBuiltinData)
import PlutusTx.Builtins (mkI)

import HelloWorld.ValidatorProxy (helloValidator, helloValidatorAddress, helloValidatorHash)

-- | REST schema
type InitHelloWorldSchema = Endpoint "initialize" Integer

type IncHelloWorldSchema = Endpoint "increment" ()
type ReadHelloWorldSchema = Endpoint "read" ()

initialize :: Contract (Last CurrencySymbol) InitHelloWorldSchema Text ()
initialize = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"initialize" initializeHandler

initializeHandler :: Integer -> Contract (Last CurrencySymbol) InitHelloWorldSchema Text ()
initializeHandler initialInt = do
  maybeMint <- mint [""]
  case maybeMint of
    Nothing -> logError @String "failed to mint"
    Just cs -> do
      let lookups = otherScript helloValidator
          tx = mustPayToOtherScript helloValidatorHash (Datum $ mkI initialInt) (singleton cs "" 1)
      adjustedTx <- adjustUnbalancedTx <$> mkTxConstraints @Void lookups tx
      ledgerTx <- submitUnbalancedTx adjustedTx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      tell $ Last $ Just cs
      logInfo $ "Successfully initialized datum with value: " <> show initialInt

mint :: [TokenName] -> Contract w s Text (Maybe CurrencySymbol)
mint tns = do
  logInfo @String $ "Starting Minting Process For " <> show tns
  pk    <- ownPaymentPubKeyHash
  logInfo @String $ "Minting NFTs to send to " <> show pk
  utxos <- utxosAt (pubKeyHashAddress pk Nothing)
  logInfo @String $ "Found " <> show (length $ Map.keys utxos) <> " UTxOs"
  case Map.keys utxos of
    [] -> do
      logError @String "No UTxO Found To use For Minting Process"
      return Nothing 
    oref : _ -> do
      let cSym            = curSymbol oref tns
          val             = mconcat $ fmap (\x -> singleton cSym x 1) tns
          lookups         = mintingPolicy (policy oref tns)
                         <> unspentOutputs utxos
          tx              = mustMintValue val
                         <> mustSpendPubKeyOutput oref
      logInfo @String $ "Submitting NFT Minting Transaction For " <> show tns
      mkTxConstraints @Void lookups tx >>= submitTxConfirmed . adjustUnbalancedTx
      logInfo @String $ "NFT Minting Transaction Confirmed For " <> show tns
      -- tell [(pk, fmap (assetClass cSym) tns)]
      return $ Just cSym


increment :: CurrencySymbol -> Contract () IncHelloWorldSchema Text ()
increment cs = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"increment" $ const $ incrementHandler cs

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

read' :: CurrencySymbol -> Contract (Last Integer) ReadHelloWorldSchema Text ()
read' cs = forever $ handleError (logError @Text) $ awaitPromise $ endpoint @"read" $ const $ readHandler cs

readHandler :: AsContractError e => CurrencySymbol -> Contract (Last Integer) s e ()
readHandler cs = do
  maybeUtxo <- findUTxOWithToken cs
  case maybeUtxo of
    Nothing ->
      logInfo @Text "Couldn't find any UTxO at the script address for the given token"
    Just (txOutRef, ciTxOut) -> do
      maybeHelloWorldDatum <- getDatum' @Integer ciTxOut
      case maybeHelloWorldDatum of
        Nothing -> do
          logInfo @Text $ "No hello world datum found at script address"
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
          logInfo @Text $ "Read datum value of " <> showText datum
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
