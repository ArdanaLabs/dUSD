module Validator
  (mainValidator
  ,mainValidatorHash
  ) where

import Plutarch.Prelude
import Plutarch.Api.V1

import Plutus.V1.Ledger.Api


mainValidator :: Term s PValidator
mainValidator = undefined

mainValidatorHash :: ValidatorHash
mainValidatorHash = validatorHash $ mkValidator mainValidator
