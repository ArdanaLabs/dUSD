module HelloWorld.AppM where

import Contract.Prelude

import Api (helloScript, redeemFromScript, sendDatumToScript, setDatumAtScript)
import Contract.Monad (Contract, liftContractAffM, liftContractM, runContract, runContract_)
import Contract.PlutusData (Datum(..), PlutusData(..), getDatumByHash)
import Contract.Transaction (TransactionInput, TransactionOutput(..))
import Contract.Utxos (getUtxo)
import Data.BigInt (fromInt, toInt, toNumber)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, ScriptAddress(..), FundsLocked(..))
import HelloWorld.Store as S
import Plutus.Types.Value (getLovelace, valueToCoin)
import Safe.Coerce (coerce)
import Scripts (validatorHash)

newtype AppM a = AppM (StoreT S.Action S.Store Aff a)

runAppM :: forall q i o. S.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store S.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore S.Action S.Store AppM

instance helloWorldApiAppM :: HelloWorldApi AppM where
  lock (ScriptAddress p) d = do
    { contractConfig } <- getStore
    (lastOutput /\ fundsLocked) <- liftAff $ runContract contractConfig $ do
      validator <- helloScript p
      vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
      lastOutput <- sendDatumToScript d vhash
      TransactionOutput utxo <- getUtxo lastOutput >>= liftContractM "couldn't find utxo"
      pure $ (lastOutput /\ (FundsLocked (toNumber ((getLovelace $ valueToCoin utxo.amount) / fromInt 1_000_000))))
    updateStore $ S.SetLastOutput lastOutput
    pure fundsLocked
  increment (ScriptAddress p) d = do
    { contractConfig, lastOutput } <- getStore
    case lastOutput of
      Nothing -> pure unit
      Just lastOutput' -> do
        lastOutput'' <- liftAff $ runContract contractConfig $ do
          validator <- helloScript p
          vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
          oldDatum <- getDatumFromState lastOutput'
          setDatumAtScript (oldDatum + d) vhash validator lastOutput'
        updateStore $ S.SetLastOutput lastOutput''
  redeem (ScriptAddress p) = do
    { contractConfig, lastOutput } <- getStore
    case lastOutput of
      Nothing -> pure unit
      Just lastOutput' -> do
        liftAff $ runContract_ contractConfig $ do
          validator <- helloScript p
          vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
          redeemFromScript vhash validator lastOutput'
        updateStore S.ResetLastOutput

getDatumFromState :: TransactionInput -> Contract () Int
getDatumFromState lastOutput = do
  TransactionOutput utxo <- getUtxo lastOutput >>= liftContractM "couldn't find utxo"
  oldDatum <-
    utxo.dataHash
      # liftContractM "utxo had not datum hash"
      >>= getDatumByHash
      >>= liftContractM "Couldn't find datum by hash"
  asBigInt <- liftContractM "datum wasn't an integer" $ case oldDatum of
    Datum (Integer n) -> Just n
    _ -> Nothing
  liftContractM "Datum was actually big. We should support this but currently don't" $ toInt asBigInt