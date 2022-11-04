{-# LANGUAGE UndecidableInstances #-}

module DUSD (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString)
import Plutarch (Config)
import Plutarch.Api.V2 (
  PMintingPolicy,
  PTxOutRef,
  PValidator,
 )
import Plutarch.Extensions.Data (parseData)
import Plutarch.Extra.TermCont (
  pguardC,
 )

trivialCbor :: Config -> Maybe String
trivialCbor = closedTermToHexString trivial

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

trivialFailCbor :: Config -> Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialFail :: ClosedTerm PValidator
trivialFail = perror

configScriptCbor :: Config -> Maybe String
configScriptCbor = closedTermToHexString configScript

configScript :: ClosedTerm PValidator
configScript = perror
-- Now a placeholder

nftCbor :: Config -> Maybe String
nftCbor = closedTermToHexString standardNft

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    pguardC "didn't spend out ref" $ pelem # outRef # inputs
    pure $ popaque $ pcon PUnit
