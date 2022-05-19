{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HelloWorld.ValidatorProxy (HelloRedeemer (..), helloValidator, helloValidatorAddress, helloValidatorHash) where

import Codec.Serialise (deserialise)
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed (embedFile)
import GHC.Generics
import Generics.SOP qualified as SOP
import Ledger (Address (..), ValidatorHash, scriptAddress, validatorHash)
import Plutus.V2.Ledger.Api (Validator (..))
import PlutusTx (makeIsDataIndexed)

import HelloWorld.ValidatorUtils (getValidatorScriptsPath)

data HelloRedeemer
  = Increment
  | Release
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

makeIsDataIndexed ''HelloRedeemer [('Increment, 0), ('Release, 1)]

helloValidatorAddress :: Address
helloValidatorAddress = scriptAddress helloValidator

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloValidator :: Validator
helloValidator = Validator . deserialise . BSL.fromStrict $ $(embedFile $ getValidatorScriptsPath ++ "/hello_world.plc")
