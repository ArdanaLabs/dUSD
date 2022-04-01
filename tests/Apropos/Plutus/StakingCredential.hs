module Apropos.Plutus.StakingCredential (
    StakingCredentialProp,
    spec,
) where

import Apropos
import Gen

import Apropos.Plutus.Credential (CredentialProp (..))
import Control.Lens (prism')
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (
    StakingCredential (..),
 )

import Test.Syd (Spec, describe)
import Test.Syd.Hedgehog (fromHedgehogGroup)

data StakingCredentialProp
    = IsHash
    | HashProp CredentialProp
    | IsPtr
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Enumerable)

instance LogicalModel StakingCredentialProp where
    logic =
        ExactlyOne [Var IsHash, Var IsPtr]
            :&&: abstractionLogic @StakingCredential

instance HasLogicalModel StakingCredentialProp StakingCredential where
    satisfiesProperty IsHash StakingHash{} = True
    satisfiesProperty IsHash _ = False
    satisfiesProperty IsPtr StakingPtr{} = True
    satisfiesProperty IsPtr _ = False
    satisfiesProperty (HashProp p) (StakingHash cred) = satisfiesProperty p cred
    satisfiesProperty (HashProp _) _ = False

instance HasAbstractions StakingCredentialProp StakingCredential where
    abstractions =
        [ WrapAbs $
            SumAbstraction
                { abstractionName = "hash"
                , propertyAbstraction = abstractsProperties HashProp
                , propLabel = IsHash
                , sumModelAbstraction =
                    prism'
                        StakingHash
                        $ \case
                            StakingHash cred -> Just cred
                            _ -> Nothing
                }
        ]

instance HasPermutationGenerator StakingCredentialProp StakingCredential where
    generators =
        abstractionMorphisms
            ++ [ Morphism
                    { name = "make StakingPtr"
                    , match = Yes
                    , contract = clear >> add IsPtr
                    , morphism = const $ StakingPtr <$> integer <*> integer <*> integer
                    }
               ]

instance HasParameterisedGenerator StakingCredentialProp StakingCredential where
    parameterisedGenerator = buildGen baseGen

baseGen :: Gen StakingCredential
baseGen =
    choice
        [ pure $ StakingPtr 0 0 0
        , StakingHash <$> genSatisfying @CredentialProp Yes
        ]

spec :: Spec
spec = do
    describe "StakingCredentialGenTests" $
        mapM_ fromHedgehogGroup $
            permutationGeneratorSelfTest
                True
                (\(_ :: Morphism StakingCredentialProp assetClassGenSelfTest) -> True)
                baseGen
