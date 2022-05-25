module Apropos.Plutus.VaultDeposit (
  VaultDepProp (..),
  VaultDepModel (..),
) where

import Apropos
import Apropos.Plutus.AssetClass (ada)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (assetClassValueOf)

data VaultDepProp
  = AdaDecreased
  | DatumChanged
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data VaultDepModel = VaultDepModel
  { input :: (Address, Value, Maybe Datum)
  , output :: (Address, Value, Maybe Datum)
  }

instance LogicalModel VaultDepProp where
  logic = Yes

instance HasLogicalModel VaultDepProp VaultDepModel where
  satisfiesProperty AdaDecreased VaultDepModel {input = (_, inVal, _), output = (_, outVal, _)} =
    assetClassValueOf outVal ada < assetClassValueOf inVal ada
  satisfiesProperty DatumChanged VaultDepModel {input = (_, _, inDatum), output = (_, _, outDatum)} =
    inDatum /= outDatum
