module ServerWallet (makeServerWallet) where

import Contract.Prelude

import Cardano.Types.Transaction (TransactionOutput(..), Utxos)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Cardano.Types.Value (Value(..), mkCoin, unwrapNonAdaAsset)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Ord.Min (Min(..))
import Serialization (publicKeyHash)
import Serialization.Address (enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Signing (getServerPubKey, serverSignTx)
import Unsafe.Coerce (unsafeCoerce)
import Wallet.Key (KeyWallet(..))

makeServerWallet :: Aff KeyWallet
makeServerWallet = do
  pubKey <- getServerPubKey
  pure $ KeyWallet
    { address : \network -> pure $
       unsafeCoerce pubKey # -- TODO is this right?
          publicKeyHash
          >>> keyHashCredential
          >>> { network, paymentCred: _ }
          >>> enterpriseAddress
          >>> enterpriseAddressToAddress
    , paymentKey : undefined -- TODO better error
    , selectCollateral : selectCollateral
    , signTx : serverSignTx pubKey
    , stakeKey : Nothing
    }


-- Taken from ctl
-- TODO this is not great
selectCollateral :: Utxos -> Maybe TransactionUnspentOutput
selectCollateral utxos = unwrap <<< unwrap <$> flip
  foldMapWithIndex
  utxos
  \input output ->
    let
      txuo = AdaOut $ TransactionUnspentOutput { input, output }
      Value ada naa = _value txuo
      onlyAda = all (all ((==) zero)) (unwrapNonAdaAsset naa)
      bigAda = ada >= mkCoin 5_000_000
    in
      if onlyAda && bigAda then Just $ Min txuo
      else Nothing

_value :: AdaOut -> Value
_value
  (AdaOut (TransactionUnspentOutput { output: TransactionOutput { amount } })) =
  amount

-- A wrapper around a UTxO, ordered by ada value
newtype AdaOut = AdaOut TransactionUnspentOutput

derive instance Newtype AdaOut _

instance Eq AdaOut where
  eq a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = eq a' b'

instance Ord AdaOut where
  compare a b
    | Value a' _ <- _value a
    , Value b' _ <- _value b = compare a' b'