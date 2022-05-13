module NFT (
  asPlutarch,
  asCurrencySymbol,
) where

import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, TxOutRef)

asPlutarch :: TxOutRef -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
asPlutarch _ = plam $ \_ _ -> popaque $ pcon PUnit

asMintingPolicy :: TxOutRef -> MintingPolicy
asMintingPolicy ref = mkMintingPolicy $ asPlutarch ref

asCurrencySymbol :: TxOutRef -> CurrencySymbol
asCurrencySymbol ref = mintingPolicySymbol $ asMintingPolicy ref
