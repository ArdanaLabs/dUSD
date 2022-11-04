module Ctl.Utils.HsmWallet (makeHsmWallet) where

import Contract.Prelude

import Contract.Address (NetworkId)
import Contract.Config (PrivatePaymentKey(..))
import Contract.Keys (mkPublicKey)
import Contract.Wallet.Key (KeyWallet(..))
import Ctl.Internal.BalanceTx.Collateral.Select as Collateral
import Ctl.Internal.Cardano.Types.Transaction (UtxoMap)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Serialization (publicKeyHash)
import Ctl.Internal.Serialization.Address (Address, enterpriseAddress, enterpriseAddressToAddress, keyHashCredential)
import Ctl.Internal.Serialization.WitnessSet (newPublicKey)
import Ctl.Utils.Signing (getPubKeyBech32, hsmSignTx)
import Data.Array (fromFoldable)
import Effect.Exception (throw)
import Unsafe.Coerce (unsafeCoerce)

-- | Returns a KeyWallet which
-- internally interfaces with the
-- signing cli
-- since the private key can't be read
-- the paymentKey method has to throw an error
makeHsmWallet :: Aff KeyWallet
makeHsmWallet = do
  bech32PubKey <- getPubKeyBech32
  serializationPubKey <- liftEffect $ newPublicKey bech32PubKey
  plutusPubKey <- case mkPublicKey bech32PubKey of
    Nothing -> liftEffect $ throw $ "Unable to create signature from bech32 string: " <> bech32PubKey
    Just key -> pure key
  let
    address :: NetworkId -> Aff Address
    address network = pure $ serializationPubKey # publicKeyHash
      >>> keyHashCredential
      >>> { network, paymentCred: _ }
      >>> enterpriseAddress
      >>> enterpriseAddressToAddress

    selectCollateral
      :: CoinsPerUtxoUnit
      -> Int
      -> UtxoMap
      -> Effect (Maybe (Array TransactionUnspentOutput))
    selectCollateral coinsPerUtxoByte maxCollateralInputs utxos = map fromFoldable
      <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs utxos

  pure $ KeyWallet
    { address
    , selectCollateral
    , signTx: hsmSignTx plutusPubKey
    , paymentKey: PrivatePaymentKey $ unsafeCoerce "tried to use the private key of a yubikey"
    , stakeKey: Nothing
    }

