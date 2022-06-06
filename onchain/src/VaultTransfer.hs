module VaultTransfer (
  vaultTransferPolicy,
) where

import Plutarch.Api.V1
import Plutarch.Prelude

import Plutus.V1.Ledger.Scripts (MintingPolicy)

vaultTransferPolicy :: MintingPolicy
vaultTransferPolicy = mkMintingPolicy vaultTransferPlutarch

vaultTransferPlutarch :: ClosedTerm PMintingPolicy
vaultTransferPlutarch = plam $ \_ _ -> popaque $ pcon PUnit