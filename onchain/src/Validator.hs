{-# OPTIONS_GHC -Wno-orphans #-}

module Validator (
  ConfigurationDatum,
  configUpdateValidator,
  mintValidator,
) where

import Plutarch

import Plutarch.Api.V1
import Plutarch.Extensions.Monad
import Plutarch.Extra.Api
import Plutarch.Extra.TermCont
import Plutarch.Prelude
import Plutarch.Unsafe

type ConfigurationDatum :: PType
type ConfigurationDatum = PBuiltinList PCurrencySymbol

configUpdateValidator :: Term s (PAsData PUnit :--> PAsData PUnit :--> PAsData PScriptContext :--> PUnit)
configUpdateValidator = signedByValidator magicPKH

magicPKH :: Term s PPubKeyHash
magicPKH = pconstant "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- TODO real key

signedByValidator :: Term s PPubKeyHash -> Term s (PAsData PUnit :--> PAsData PUnit :--> PAsData PScriptContext :--> PUnit)
signedByValidator key = plam $ \_ _ sc -> unTermCont $ do
  PScriptContext sc' <- pmatchC $ pfromData sc
  txinfo <- pletFieldC @"txInfo" sc'
  signatures <- pletFieldC @"signatories" txinfo
  pguardC "not signed by key" $ pelem # key #$ pmap # plam pfromData # signatures
  pure $ pcon PUnit

configID :: Term s PTxId
configID = pconstant "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

-- TODO I think this should just be the validatorHash?

mintValidator :: Term s (PAsData PUnit :--> PAsData PInteger :--> PAsData PScriptContext :--> PUnit)
mintValidator = plam $ \_ n sc -> unTermCont $ do
  PScriptContext sc' <- pmatchC $ pfromData sc
  txinfo <- pletFieldC @"txInfo" sc'
  rec <- pletFieldsC @'["mint", "inputs", "datums"] txinfo
  let minting = getField @"mint" rec
  let inputs = getField @"inputs" rec
  let datumTable = getField @"datums" rec
  -- TODO I'm not sure the next line is correct
  PJust configInput <- pmatchC $ pfind # plam (\t -> pfromData (pfield @"id" # pfromData (pfield @"outRef" # pfromData t)) #== configID) # pfromData inputs
  PDJust configDatumHashRecord <- pmatchC $ pfield @"datumHash" #$ pfromData $ pfield @"resolved" # configInput
  configDatumHash <- pletC $ pfromData $ pfield @"_0" # configDatumHashRecord
  -- TODO
  -- using PByteString and unsafeCoerce
  -- is a temporary fix for PCurrencySymbol and PTokenName not having
  -- PTryFrom PData instances
  PJust configAsData <- pmatchC $ pparseDatum @(PBuiltinPair (PAsData PByteString) (PAsData PByteString)) # configDatumHash # datumTable
  let config = pfromData configAsData
  let mustMintThis = pfromData $ pelemAt @PBuiltinList # pfromData n # punsafeCoerce config
  pguardC "indexed token not minted" $ containsPosAmt # mustMintThis # minting
  pure $ pcon PUnit

containsPosAmt :: Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName) :--> PValue :--> PBool)
containsPosAmt = plam $ \ac val -> unTermCont $ do
  PValue m <- pmatchC val
  PMap l <- pmatchC m
  pmatchC (pfind # plam (\p -> pfromData (pfstBuiltin # p) #== pfromData (pfstBuiltin # ac)) # l) >>= \case
    PNothing -> pure $ pcon PFalse
    PJust te -> do
      PMap tokens <- pmatchC $ pfromData $ psndBuiltin # te
      pure $
        pany
          # plam
            ( \p ->
                pfromData (pfstBuiltin # p) #== pfromData (psndBuiltin # ac)
                  #&& 0 #< pfromData (psndBuiltin # p)
            )
          # tokens
