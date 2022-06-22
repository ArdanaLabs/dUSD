module UnitTest
  (helloUnitTest
  ) where

import Contract.Prelude

import Api(helloScript, payToHello, incHello, redeemFromHello)

import Contract.Monad (Contract, liftContractM, logInfo')
import Contract.Scripts (validatorHash)

helloUnitTest :: Contract () Unit
helloUnitTest = do
  logInfo' "Running Examples.Hello"
  validator <- helloScript 4
  vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
  payToHello 5 vhash >>= incHello 9 vhash validator >>= redeemFromHello vhash validator

