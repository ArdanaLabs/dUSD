module Apropos.Plutus.Address (
  AddressProp (..),
  spec,
) where

import Apropos
import Apropos.Plutus.Credential (CredentialProp)
import Apropos.Plutus.Maybe (MaybeProp (..))
import Apropos.Plutus.StakingCredential (StakingCredentialProp)
import Control.Lens (lens)
import Plutus.V1.Ledger.Api (Address (..))

import Test.Syd (Spec, describe)
import Test.Syd.Hedgehog (fromHedgehogGroup)

data AddressProp
  = Cred CredentialProp
  | StakingCred (MaybeProp StakingCredentialProp)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel AddressProp where
  logic = abstractionLogic @Address

instance HasLogicalModel AddressProp Address where
  satisfiesProperty (Cred p) (Address c _) = satisfiesProperty p c
  satisfiesProperty (StakingCred p) (Address _ sc) = satisfiesProperty p sc

instance HasAbstractions AddressProp Address where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "credential"
          , propertyAbstraction = abstractsProperties Cred
          , productModelAbstraction =
              lens
                (\(Address adr _) -> adr)
                (\(Address _ msc) adr -> Address adr msc)
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "maybe stakingcredential"
          , propertyAbstraction = abstractsProperties StakingCred
          , productModelAbstraction =
              lens
                (\(Address _ msc) -> msc)
                (\(Address adr _) msc -> Address adr msc)
          }
    ]

instance HasPermutationGenerator AddressProp Address where
  generators = abstractionMorphisms

instance HasParameterisedGenerator AddressProp Address where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Address
baseGen =
  Address
    <$> genSatisfying @CredentialProp Yes
    <*> genSatisfying @(MaybeProp StakingCredentialProp) Yes

spec :: Spec
spec = do
  describe "AddressGenSelfTests" $
    mapM_ fromHedgehogGroup $
      permutationGeneratorSelfTest
        True
        (\(_ :: Morphism AddressProp a) -> True)
        baseGen
