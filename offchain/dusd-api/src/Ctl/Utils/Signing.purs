module Ctl.Utils.Signing
  ( getPubKeyBech32
  , hsmSignTx
  ) where

import Contract.Prelude

import Contract.Address (ByteArray, Bech32String)
import Contract.Keys (mkEd25519Signature)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction (PublicKey, Transaction(Transaction), TransactionWitnessSet, Vkey(..), _vkeys)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization as Serialization
import Data.Lens (set)
import Data.String (trim)
import Effect.Exception (throw)
import Node.Buffer.Class (toString)
import Node.ChildProcess (defaultExecSyncOptions, execFileSync)
import Node.Encoding (Encoding(UTF8))
import Untagged.Union (asOneOf)

-- | This type is intended to represent a generic foreign signing function
type Signer = ByteArray -> Aff String

getPubKeyBech32 :: Aff Bech32String
getPubKeyBech32 = trim <$> execAff "signer" [ "getPubKey" ]

hsmSignTx :: PublicKey -> Transaction -> Aff TransactionWitnessSet
hsmSignTx = signTx cmdSigner

cmdSigner :: Signer
cmdSigner tx = do
  let args = [ "sign", byteArrayToHex tx ]
  execAff "signer" args <#> trim

execAff :: String -> Array String -> Aff String
execAff cmd args = liftEffect $ execFileSync cmd args defaultExecSyncOptions >>= toString UTF8

signTx :: Signer -> PublicKey -> Transaction -> Aff TransactionWitnessSet
signTx signer pubKey (Transaction tx) = do
  txBody <- liftEffect $ Serialization.convertTxBody tx.body
  hash <- liftEffect $ Serialization.hashTransaction txBody
  sig <- signer $ toBytes $ asOneOf hash
  case mkEd25519Signature sig of
    Nothing -> liftEffect $ throw $ "Unable to create signature from value: " <> sig
    Just s -> do
      let wit = Just [ wrap $ (Vkey pubKey) /\ (s) ]
      pure $ set _vkeys wit mempty
