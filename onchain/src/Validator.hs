module Validator (
  ConfigurationDatum,
  nftMintingPolicy,
  nftCS,
  mainValidator,
  mainAddress,
) where



import Plutarch
import Plutarch.Prelude

import Plutarch.Api.V1
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad
import Plutarch.Extra.Api
import Plutarch.Extra.TermCont
import Plutarch.Prelude
import Plutarch.Unsafe
import Plutus.V1.Ledger.Api

type ConfigurationDatum :: PType
type ConfigurationDatum = PBuiltinList PCurrencySymbol

mainValidator :: Term s PValidator
mainValidator = plam $ \_ _ _ -> popaque $ pcon PUnit

mainValidatorHash :: ValidatorHash
mainValidatorHash = validatorHash $ mkValidator mainValidator
-- TODO move this to an initial config thing
_magicPKH :: Term s PPubKeyHash
_magicPKH = pconstant "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- TODO real key

_signedByValidator :: Term s PPubKeyHash -> Term s (PAsData PUnit :--> PAsData PUnit :--> PAsData PScriptContext :--> PUnit)
_signedByValidator key = plam $ \_ _ sc -> unTermCont $ do
  PScriptContext sc' <- pmatchC $ pfromData sc
  txinfo <- pletFieldC @"txInfo" sc'
  signatures <- pletFieldC @"signatories" txinfo
  pguardC "not signed by key" $ pelem # key #$ pmap # plam pfromData # signatures
  pure $ pcon PUnit

nftMintingPolicy :: Term s PTxOutRef -> Term s (PData :--> PScriptContext :--> POpaque)
nftMintingPolicy outRef = plam $ \_ sc' -> unTermCont $ do
  PScriptContext sc <- pmatchC sc'
  scRec <- pletFieldsC @'["txInfo", "purpose"] sc
  rec <- pletFieldsC @'["mint", "inputs"] $ getField @"txInfo" scRec
  pguardC "didn't spend outRef" $ pelem # outRef #$ (pmap # pfield @"outRef" #$ pfromData (getField @"inputs" rec))
  PMinting csData <- pmatchC $ getField @"purpose" scRec
  let cs = pfield @"_0" # csData
  PValue m <- pmatchC $ pfromData (getField @"mint" rec)
  PMap l <- pmatchC m
  let h = unsingleton l
  pguardC "minting wrong currency symobol" $ pfstBuiltin # h #== cs
  let tokenMap = psndBuiltin # h
  PMap tokenList <- pmatchC $ pfromData tokenMap
  let singleToken = unsingleton tokenList
  pguardC "minted not exactly one" $ psndBuiltin # singleToken #== pdata 1
  pure $ popaque (pcon PUnit)

nftCS :: ClosedTerm PTxOutRef -> CurrencySymbol
nftCS outRef = mintingPolicySymbol $ mkMintingPolicy $ nftMintingPolicy outRef

mainValidator :: ClosedTerm PTxOutRef -> Term s (PData :--> PData :--> PScriptContext :--> POpaque)
mainValidator outRef = plam $ \_ n' sc' -> unTermCont $ do
  PScriptContext sc <- pmatchC sc'
  scRec <- pletFieldsC @'["txInfo", "purpose"] sc
  rec <- pletFieldsC @'["mint", "inputs", "datums", "outputs"] $ getField @"txInfo" scRec
  let minting = getField @"mint" rec
  inputs <- pletC $ pfromData $ getField @"inputs" rec
  let datumTable = getField @"datums" rec
  PSpending spending <- pmatchC $ getField @"purpose" scRec
  PJust ownInput <- pmatchC $ pfindOwnInput # inputs # (pfield @"_0" # spending)
  -- TODO check for nft at config input
  inputResolved <- pletFieldC @"resolved" ownInput
  -- we require the token to be "" for now
  nftAC <- pletC $ ppairDataBuiltin # pdata (pconstant $ nftCS outRef) # pdata (pcon $ PTokenName $ pconstant "")
  PJust configInput <- pmatchC $ pfind # plam (\i -> containsPosAmt # nftAC #$ pfield @"value" #$ pfield @"resolved" # pfromData i) # inputs
  PDJust configDatumHashRecord <- pmatchC $ pfield @"datumHash" #$ pfield @"resolved" # pfromData configInput
  let configDatumHash = pfield @"_0" # configDatumHashRecord
  PJust configAsData <- pmatchC $ pparseDatum @(PBuiltinPair (PAsData PByteString) (PAsData PByteString)) # configDatumHash # datumTable
  let config = pfromData configAsData
  -- TODO ideally this would use ptryFromData (not punsafeCoerce) but it's not exported
  let n = pfromData $ punsafeCoerce n'
  -- Because we use the redemer as the config index for
  -- all inputs all the inputs (from our address) must have the same redemer of the index of the config rule
  let mustMintThis = pfromData $ pelemAt @PBuiltinList # n # punsafeCoerce config
  pguardC "indexed token not minted" $ containsPosAmt # mustMintThis # minting
  pguardC "altered config illegally" $
    -- either the redemer is 0 which is reserved for updating config
    (n #== 0)
      #||
      -- or some output carries the nft forrward at the same address and the same datumHash
      pany
      # plam ((#== inputResolved) . pfromData)
      # pfromData (getField @"outputs" rec)
  pure $ popaque $ pcon PUnit

containsPosAmt :: Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName) :--> PValue :--> PBool)
containsPosAmt = plam $ \ac val -> unTermCont $ do
  PValue m <- pmatchC val
  PMap l <- pmatchC m
  pmatchC (pfind # plam (\p -> (pfstBuiltin # p) #== (pfstBuiltin # ac)) # l) >>= \case
    PNothing -> pure $ pcon PFalse
    PJust te -> do
      PMap tokens <- pmatchC $ pfromData $ psndBuiltin # te
      pure $
        pany
          # plam
            ( \p ->
                (pfstBuiltin # p) #== (psndBuiltin # ac)
                  #&& 0 #< pfromData (psndBuiltin # p)
            )
          # tokens

mainAddress :: ClosedTerm PTxOutRef -> ValidatorHash
mainAddress outRef = validatorHash $ mkValidator (mainValidator outRef)
