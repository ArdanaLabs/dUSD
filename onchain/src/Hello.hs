{-# LANGUAGE UndecidableInstances #-}

module Hello (
  HelloRedeemer (..),
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
) where

import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)

import Plutarch.Api.V1 (PScriptContext, mkValidator, validatorHash)
import Plutarch.DataRepr
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Extra.TermCont
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

data HelloRedeemer (s :: S)
  = Increment (Term s (PDataRecord '[]))
  | Release (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PEq, POrd)
    via PIsDataReprInstances HelloRedeemer

helloValidator :: Validator
helloValidator = mkValidator validator

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

validator :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque)
validator = plam $ \dn dredemer dsc -> do
  let n = pfromData (punsafeCoerce @_ @PData @(PAsData PInteger) dn)
      r = pfromData (punsafeCoerce @_ @PData @(PAsData HelloRedeemer) dredemer)
      res = validator' # n # r # dsc
   in popaque res

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

validator' :: ClosedTerm (PInteger :--> HelloRedeemer :--> PScriptContext :--> PUnit)
validator' = plam $ \n redemer sc -> unTermCont $ do
  pmatchC redemer >>= \case
    Increment _ -> do
      -- since release won't have a continuingDatum and this can fail
      -- we need to run this inside the increment case
      datum <- pgetContinuingDatum @PInteger sc
      pure $ helloLogic # n # pfromData datum
    Release _ -> pure $ pcon PUnit

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PUnit)
helloLogic = plam $ \n m -> unTermCont $ passert "int was not correct" $ n + 1 #== m
