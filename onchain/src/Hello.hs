module Hello (helloScript, helloLogic) where

import Plutus.V1.Ledger.Scripts (Script)

import Plutarch (compile)
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Prelude

helloScript :: Script
helloScript = compile validator

validator :: ClosedTerm (PAsData PInteger :--> PAsData PUnit :--> PAsData PScriptContext :--> PUnit)
validator = plam $ \dn dunit dsc -> validator' # pfromData dn # pfromData dunit # pfromData dsc

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

validator' :: ClosedTerm (PInteger :--> PUnit :--> PScriptContext :--> PUnit)
validator' = plam $ \n _unit sc ->
  unTermCont $ passert "int was not correct" <$> (n + 1 #==) =<< getContinuingDatum sc

-- TODO: remove this and test validator instead
helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PBool)
helloLogic = plam $ \n m -> n + 1 #== m
