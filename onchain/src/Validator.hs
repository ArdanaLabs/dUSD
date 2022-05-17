module Validator (
  mainValidator,
  mainValidatorHash,
) where

import Plutarch.Api.V1
import Plutarch.Prelude

import Plutus.V1.Ledger.Api

mainValidator :: Term s PValidator
mainValidator = plam $ \_ _ _ -> popaque $ pcon PUnit

mainValidatorHash :: ValidatorHash
mainValidatorHash = validatorHash $ mkValidator mainValidator
