{-# LANGUAGE UndecidableInstances #-}

module Hello (
  helloConfig,
  helloValidator,
  helloLogic,
  helloValidatorHash,
  helloAddress,
  helloWorldCbor,
  paramHelloCbor,
  trivialCbor,
  trivialFailCbor,
  trivialSerialise,
  HelloRedemer (..),
) where

import Utils (closedTermToHexString, validatorToHexString)

import PlutusLedgerApi.V2 (Validator, ValidatorHash)

import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Credential (Credential (..))

import Plutarch.Prelude

import Plutarch.Api.V2 (PScriptContext, PValidator, mkValidator, validatorHash)
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Extensions.Api (passert, pgetContinuingDatum)
import Plutarch.Unsafe (punsafeCoerce)

import Data.Default (Default (..))
import Plutarch (Config (tracingMode), TracingMode (..))
import Plutarch.Extra.TermCont (pmatchC)

data HelloRedemer (s :: S)
  = Inc (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType HelloRedemer where type DPTStrat _ = PlutusTypeData

helloConfig :: Config
helloConfig = def {tracingMode = NoTracing}

helloWorldCbor :: String
helloWorldCbor = validatorToHexString helloValidator

paramHelloCbor :: Config -> Maybe String
paramHelloCbor = closedTermToHexString paramValidator

trivialCbor :: Config -> Maybe String
trivialCbor = closedTermToHexString trivial

trivialFailCbor :: Config -> Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialSerialise :: Config -> Maybe String
trivialSerialise = closedTermToHexString (plam $ \a _ _ -> pserialiseData # a)

helloValidator :: Validator
helloValidator = mkValidator helloConfig (paramValidator #$ pforgetData (pdata (1 :: Term _ PInteger)))

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloAddress :: Address
helloAddress = Address (ScriptCredential helloValidatorHash) Nothing

trivialFail :: ClosedTerm PValidator
trivialFail = perror

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

paramValidator :: ClosedTerm (PData :--> PValidator)
paramValidator = plam $ \dCountBy dn dunit dsc -> do
  let n = pfromData (punsafeCoerce dn)
      u = pfromData (punsafeCoerce dunit)
      res cb = paramValidator' # cb # n # u # dsc
   in ptryFrom @(PAsData PInteger) dCountBy $ \(_, i) -> res i

-- TODO Try wrapping the counter in a newtype to
-- test shareing newtypes/datatypes with apps

paramValidator' :: ClosedTerm (PInteger :--> PInteger :--> HelloRedemer :--> PScriptContext :--> POpaque)
paramValidator' = plam $ \countBy n r sc -> unTermCont $ do
  pmatchC r >>= \case
    Inc _ -> do
      datum <- pgetContinuingDatum @PInteger sc
      pure $ helloLogic # countBy # n # datum
    Spend _ -> pure $ popaque $ pcon PUnit

helloLogic :: ClosedTerm (PInteger :--> PInteger :--> PInteger :--> POpaque)
helloLogic = plam $ \countBy n m -> unTermCont $ passert "int was not correct" $ n + countBy #== m
