{-# LANGUAGE UndecidableInstances #-}

module Hello (
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
  helloWorldHexString,
  HelloRedemer (..),
  incRedemer,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Utils (validatorToHexString)

import Plutus.V1.Ledger.Api

import Plutarch.Prelude

import Plutarch.Api.V1 (PScriptContext, mkValidator, validatorHash)
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Unsafe (punsafeCoerce)

incRedemer :: Data
incRedemer = plift $ pforgetData $ pdata $ pcon $ Inc pdnil

data HelloRedemer (s :: S)
  = Inc (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PEq)
    via (PIsDataReprInstances HelloRedemer)

helloWorldHexString :: String
helloWorldHexString = validatorToHexString helloValidator

helloValidator :: Validator
helloValidator = mkValidator validator

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

validator :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque)
validator = plam $ \dn dunit dsc -> do
  let n = pfromData (punsafeCoerce dn)
      u = pfromData (punsafeCoerce dunit)
      res = validator' # n # u # dsc
   in popaque res

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

validator' :: ClosedTerm (PInteger :--> HelloRedemer :--> PScriptContext :--> PUnit)
validator' = plam $ \n r sc -> unTermCont $ do
  pmatchC r >>= \case
    Inc _ -> do
      datum <- pgetContinuingDatum @PInteger sc
      pure $ helloLogic # n # pfromData datum
    Spend _ ->
      pure $ pcon PUnit

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PUnit)
helloLogic = plam $ \n m -> unTermCont $ passert "int was not correct" $ n + 1 #== m
