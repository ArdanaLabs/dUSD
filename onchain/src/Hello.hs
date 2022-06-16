module Hello (
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
  helloWorldHexString,
  paramHelloCBOR,
) where

import Utils (validatorToHexString,closedTermToHexString)

import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)

import Plutarch.Api.V1 (PValidator,PScriptContext, mkValidator, validatorHash)
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

helloWorldHexString :: String
helloWorldHexString = validatorToHexString helloValidator

paramHelloCBOR :: String
paramHelloCBOR = closedTermToHexString paramValidator

helloValidator :: Validator
helloValidator = mkValidator (paramValidator # 1)

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

paramValidator :: ClosedTerm (PInteger :--> PValidator)
paramValidator = plam $ \countBy dn dunit dsc -> do
  let n = pfromData (punsafeCoerce dn)
      u = pfromData (punsafeCoerce dunit)
      res = paramValidator' # countBy # n # u # dsc
   in popaque res

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

paramValidator' :: ClosedTerm (PInteger :--> PInteger :--> PUnit :--> PScriptContext :--> PUnit)
paramValidator' = plam $ \countBy n _unit sc -> unTermCont $ do
  datum <- pgetContinuingDatum @PInteger sc
  pure $ helloLogic # countBy # n # pfromData datum

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PInteger :--> PUnit)
helloLogic = plam $ \countBy n m -> unTermCont $ passert "int was not correct" $ n + countBy #== m
