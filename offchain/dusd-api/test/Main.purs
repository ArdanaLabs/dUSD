module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftContractM)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import Ctl.Utils.Test (expectScriptError, runWithMode, useRunnerSimple)
import Ctl.Utils.Test.Types (Mode(..))
import DUSD.Api (mintNft, seedTx)
import DUSD.CborTyped (simpleNft)
import Data.BigInt as BigInt
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Spec (describe, it, parallel, sequential)
import Test.Spec.Assertions (expectError, shouldEqual)

main :: Effect Unit
main = launchAff_ $ do
  mode <- liftEffect $ lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> do
      -- TODO add this back when we have an API key again
      -- topup "addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc"
      pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  log "About to start tests"
  let maybePar = if mode == Local then parallel else sequential
  runWithMode mode $ do

    describe "NFT" $ maybePar $ do

      it "Mints an NFT with the seed UTxO as an input" $ useRunnerSimple do
        mintNft

      it "Cannot mint with the seed UTxO as a reference input" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustReferenceOutput txOut
        expectScriptError $ buildBalanceSignAndSubmitTx lookups constraints

      it "Double minting fails on second mint" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        _ <- waitForTx adr txId
        -- It's fine and expected that this is not a script error
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "Spends the seed UTxO after minting" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "No wallet" =<< getWalletAddress
        utxos <- getUtxos adr
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
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        _ <- waitForTx adr txId
        getUtxo txOut >>= case _ of
          Nothing -> pure unit
          Just _ -> liftEffect $ throw "seed tx still existed"

      it "Sends the NFT to the wallet after minting" $ useRunnerSimple do
        cs <- mintNft
        bal <- liftContractM "no ballance" =<< getWalletBalance
        let nfts = Value.valueOf bal cs adaToken
        nfts `shouldEqual` (BigInt.fromInt 1)

      it "Cannot burn an NFT" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        let
          mintLookups :: Lookups.ScriptLookups PlutusData
          mintLookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          mintConstraints :: TxConstraints Unit Unit
          mintConstraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustSpendPubKeyOutput txOut
        txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
        _ <- waitForTx adr txid
        let
          burnLookups :: Lookups.ScriptLookups PlutusData
          burnLookups = Lookups.mintingPolicy nftPolicy

          burnConstraints :: TxConstraints Unit Unit
          burnConstraints =
            Constraints.mustSpendPubKeyOutput txOut
              <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
        expectError
          $ buildBalanceSignAndSubmitTx burnLookups burnConstraints

