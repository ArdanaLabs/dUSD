{-# LANGUAGE UndecidableInstances #-}

module VaultDeposit (
  vaultDepositPolicy,
  VaultDatum (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1
import Plutarch.DataRepr
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (MintingPolicy)

-- TODO this will probably move during the actual implementation
-- but I think it's fine here for tests
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

vaultDepositPolicy :: MintingPolicy
vaultDepositPolicy = mkMintingPolicy vaultDepositPolicyPlutarch

vaultDepositPolicyPlutarch :: Term s PMintingPolicy
vaultDepositPolicyPlutarch = plam $ \_ _ -> popaque $ pcon PUnit
