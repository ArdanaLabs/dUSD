module Apropos.Plutus.MainValidator (
  ValidatorProp (..),
  ValidatorModel (..),
) where

import Apropos
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Tx

data ValidatorProp
  = HasConfig
  | ConfigHasNFT
  | RedemerIsValid
  | MintsReferencedToken
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

data ValidatorModel = ValidatorModel
  { minting :: Value
  , inputs :: [TxIn]
  , redemer :: Either Int Data
  }

instance LogicalModel ValidatorProp where
  logic =
    (Var ConfigHasNFT :->: Var HasConfig)
      :&&: (Var MintsReferencedToken :->: Var RedemerIsValid)
