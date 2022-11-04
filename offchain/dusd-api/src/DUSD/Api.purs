module DUSD.Api
  ( mintNft
  , seedTx
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson')
import Contract.Address (getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Hashing (datumHash)
import Contract.Log (logDebug', logError', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (class FromData, class ToData, Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput(..), TransactionHash(..), TransactionOutputWithRefScript)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, TokenName, adaToken, mkTokenName, mpsSymbol, scriptCurrencySymbol, symbols, valueOf)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DUSD.CborTyped (configAddressValidator, simpleNft)
import Data.BigInt (BigInt, toNumber)
import Data.BigInt as BigInt
import Data.Int (floor)
import Data.List (head)
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Data.UInt as U
import Effect.Exception (throw)
import Math (sqrt)

-- | Mints an nft where the txid is a parameter of the contract and returns the currency symbol
mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logDebug' $ "seed tx id was:" <> show txOut
  nftPolicy <- simpleNft txOut
  cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
      <> Constraints.mustSpendPubKeyOutput txOut
  logDebug' "about to submit"
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  logDebug' "submitted"
  _ <- waitForTx adr txId
  pure $ cs

-- | Selects a utxo owned by the current wallet usefull for minting nfts
seedTx :: Contract () TransactionInput
seedTx = do
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logInfo' $ show adr
  logInfo' $ "utxos: " <> show utxos
  col <- fromMaybe [] <$> getWalletCollateral
  logInfo' $ "col: " <> show col
  let colIns = (unwrap >>> _.input) <$> col
  logInfo' $ "colIns: " <> show colIns
  let nonColateralUtxOs = Map.filterKeys (\utxo -> utxo `notElem` colIns) utxos
  logInfo' $ "nonColUtxos: " <> show nonColateralUtxOs
  sending <- case head $ toUnfoldable $ keys nonColateralUtxOs of
    Just sending -> pure sending
    Nothing -> do
      logInfo' "all utxos were collateral using collateral utxo"
      liftContractM "no utxos at all" $ head $ toUnfoldable $ keys utxos
  logInfo' $ "sending: " <> show sending
  out <- liftContractM "no output" =<< getUtxo sending
  logInfo' $ "out: " <> show out
  pure sending
