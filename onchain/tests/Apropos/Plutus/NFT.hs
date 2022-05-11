module Apropos.Plutus.NFT (
  NFTModel (..),
  NFTProp (..),
) where

import Apropos
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

data NFTModel = NFTModel
  { inputs :: [TxInInfo]
  , outputs :: [TxOut]
  , signatures :: [PubKeyHash]
  }

data NFTProp
  = SpendsRightInput
  | SignedByMagicKey
  | MintsNft
  | MintsTooMany
  | MintsManyTimes
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

instance LogicalModel NFTProp where
  logic = Yes

instance HasLogicalModel NFTProp NFTModel where
  satisfiesProperty MintsNft m = any hasNft $ outputs m
  satisfiesProperty SignedByMagicKey m = magicKey `elem` signatures m
  satisfiesProperty MintsTooMany m = length (filter hasNft (outputs m)) >= 2
  satisfiesProperty MintsManyTimes m = any hasTooMuchNft $ outputs m
  satisfiesProperty SpendsRightInput m = any isMagicInput $ inputs m

hasNft :: TxOut -> Bool
hasNft (TxOut _ v _) = valueOf v magicNFT "" > 0

hasTooMuchNft :: TxOut -> Bool
hasTooMuchNft (TxOut _ v _) = valueOf v magicNFT "" >= 2

isMagicInput :: TxInInfo -> Bool
isMagicInput (TxInInfo (TxOutRef txid _) _) = txid == inputId

inputId :: TxId
inputId = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

magicNFT :: CurrencySymbol
magicNFT = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

magicKey :: PubKeyHash
magicKey = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
