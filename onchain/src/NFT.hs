module NFT (
  nftPolicyPlutarch,
  nftMintingPolicy,
  nftCs,
) where

import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Prelude
import Plutus.V1.Ledger.Api (CurrencySymbol, MintingPolicy, TxOutRef)

nftPolicyPlutarch :: TxOutRef -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
nftPolicyPlutarch _ = plam $ \_ _ -> popaque $ pcon PUnit

nftMintingPolicy :: TxOutRef -> MintingPolicy
nftMintingPolicy ref = mkMintingPolicy $ nftPolicyPlutarch ref

nftCs :: TxOutRef -> CurrencySymbol
nftCs ref = mintingPolicySymbol $ nftMintingPolicy ref
