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
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, TxId)

asPlutarch :: TxId -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
asPlutarch _ = plam $ \_ _ -> popaque $ pcon PUnit

asMintingPolicy :: TxId -> MintingPolicy
asMintingPolicy txid = mkMintingPolicy $ asPlutarch txid

asCurrencySymbol :: TxId -> CurrencySymbol
asCurrencySymbol txid = mintingPolicySymbol $ asMintingPolicy txid
