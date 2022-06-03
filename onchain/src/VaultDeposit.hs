module VaultDeposit (
  vaultDepositPolicy,
) where

import Plutarch.Api.V1
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (MintingPolicy)

vaultDepositPolicy :: MintingPolicy
vaultDepositPolicy = mkMintingPolicy vaultDepositPolicyPlutarch

vaultDepositPolicyPlutarch :: Term s PMintingPolicy
vaultDepositPolicyPlutarch = plam $ \_ _ -> popaque $ pcon PUnit
