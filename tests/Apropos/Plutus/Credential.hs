module Apropos.Plutus.Credential (
  CredentialProp (..),
  spec,
) where

import Apropos
import Gen

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (Credential (..))

import Test.Syd (Spec, describe)
import Test.Syd.Hedgehog (fromHedgehogGroup)

data CredentialProp
  = IsPubKey
  | IsScript
  -- TODO props for particular scripts
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel CredentialProp where
  logic = ExactlyOne [Var IsPubKey, Var IsScript]

instance HasLogicalModel CredentialProp Credential where
  satisfiesProperty IsPubKey PubKeyCredential {} = True
  satisfiesProperty IsPubKey _ = False
  satisfiesProperty IsScript ScriptCredential {} = True
  satisfiesProperty IsScript _ = False

instance HasPermutationGenerator CredentialProp Credential where
  generators =
    [ Morphism
        { name = "make pubkey"
        , match = Yes
        , contract = clear >> add IsPubKey
        , morphism = const $ PubKeyCredential <$> pubKeyHash
        }
    , Morphism
        { name = "make script"
        , match = Yes
        , contract = clear >> add IsScript
        , morphism = const $ ScriptCredential <$> validatorHash
        }
    ]

baseGen :: Gen Credential
baseGen =
  choice
    [ PubKeyCredential <$> pubKeyHash
    , ScriptCredential <$> validatorHash
    ]

instance HasParameterisedGenerator CredentialProp Credential where
  parameterisedGenerator = buildGen baseGen

spec :: Spec
spec = do
  describe "CredentialModelTests" $
    mapM_ fromHedgehogGroup $
      permutationGeneratorSelfTest
        True
        (\(_ :: Morphism CredentialProp assetClassGenSelfTest) -> True)
        baseGen
