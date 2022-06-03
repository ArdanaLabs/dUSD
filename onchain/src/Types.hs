{-# LANGUAGE UndecidableInstances #-}

module Types (
  VaultDatum (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude

newtype VaultDatum (s :: S)
  = VaultDatum
      ( Term
          s
          ( PDataRecord
              '[ "debt" ':= PInteger
               , "authorisation" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields, PEq, POrd)
    via PIsDataReprInstances VaultDatum
