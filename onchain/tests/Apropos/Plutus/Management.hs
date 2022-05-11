module Apropos.Plutus.Auction (
  spec,
) where

import Apropos
import GHC.Generics (Generic)
import Gen
import Test.Syd
import Test.Syd.Hedgehog

import Plutarch (compile, (#))
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude qualified as PPrelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue)

-- | The model for the properties.
type ManagementModel = ManagementModel
  { mmCurrencies :: [CurrencySymbol]
  , mmSignatures :: [PubKeyHash]
  , mmCurChoice :: Int
  , mmMinted :: Value
  , mmOwner  :: PubKeyHash
  , mmInput  :: [TxInInfo]
  , mmOutput :: [TxOut]
  } deriving stock (Show, Eq, Generic)

data ManagementProp
  = BeenSigned
  | MintsOne
  | MintsValidChoice -- i.e. 0 <= mmCurChoice < length mmCurrencies
  | MintsCorrectly
  | ConfigPresent
  | ConfigReturned
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Enumerable ManagementProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel ManagementProp where
  logic = 


