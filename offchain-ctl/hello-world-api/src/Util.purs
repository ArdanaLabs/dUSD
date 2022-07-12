module Util
  (waitForTx
  ,buildBalanceSignAndSubmitTx
  ,getUtxos
  ,rapplyUnwrap
  ,(#%)
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad
  ( Contract
  , liftedE
  , liftedM
  , logInfo'
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput
  , TransactionInput(TransactionInput)
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoM(UtxoM), utxosAt)


import Data.Map (Map)
import Data.Map as Map
import Data.Time.Duration
  (Milliseconds(..)
  ,Seconds(..)
  ,class Duration
  ,fromDuration
  ,convertDuration
  ,negateDuration
  )
import Effect.Aff (delay)
import Types.PlutusData (PlutusData)

waitForTx
  :: forall (a :: Type) (r :: Row Type).
  Duration a
  => a
  -> ValidatorHash
  -> TransactionHash
  -> Contract r (Maybe TransactionInput)
waitForTx d vhash txid = do
  let hasTransactionId :: TransactionInput /\ TransactionOutput -> Boolean
      hasTransactionId (TransactionInput tx /\ _) =
        tx.transactionId == txid
  utxos <- getUtxos vhash
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array (TransactionInput /\ TransactionOutput)) of
      Nothing ->
        if (fromDuration d <= (Milliseconds 0.0))
          then do
            pure Nothing
          else do
              logInfo' $ "No tx yet, waiting for: " <> show (convertDuration d :: Seconds)
              (liftAff <<< delay <<< wrap) 1000.0
              waitForTx (fromDuration d <> fromDuration (negateDuration (Seconds 1.0))) vhash txid
      Just txin -> do
        logInfo' $ "found tx:" <> show txid
        pure $ Just txin

buildBalanceSignAndSubmitTx
  :: forall (r :: Row Type).
  Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract r TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ balanceAndSignTxE ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

getUtxos :: forall (r :: Row Type). ValidatorHash -> Contract r (Map TransactionInput TransactionOutput)
getUtxos vhash = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  pure utxos

-- | Reverse function application on 
-- | a wrapped newtype. Intended to
-- | be used
rapplyUnwrap :: forall (new :: Type) (old :: Type) (a :: Type). (Newtype new old) => new -> (old -> a) -> a
rapplyUnwrap val f = f (unwrap val)

infixl 1 rapplyUnwrap as #%

