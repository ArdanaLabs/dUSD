module Apropos.Plutus.StakingCredential
  ( StakingCredentialProp
  , spec
  ) where

import Apropos
import Apropos.LogicalModel
import Gen

import Apropos.Plutus.Credential (CredentialProp(..))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (
  StakingCredential(..),
  Credential,
                            )
import qualified Data.Map as M
import Control.Lens

import Test.Syd
import Test.Syd.Hedgehog ( fromHedgehogGroup )

data StakingCredentialProp
  = IsHash
  | HashProp CredentialProp
  | IsPtr
  deriving stock (Eq,Ord,Show,Generic)
  deriving anyclass Enumerable

instance LogicalModel StakingCredentialProp where
  logic =
    ExactlyOne [ Var IsHash , Var IsPtr ]
    :&&: (Var IsHash :->: HashProp <$> logic)
    :&&: (Var IsPtr :->: None (Var . HashProp <$> enumerated))

instance HasLogicalModel StakingCredentialProp StakingCredential where
  satisfiesProperty IsHash StakingHash{} = True
  satisfiesProperty IsHash _ = False
  satisfiesProperty IsPtr StakingPtr{} = True
  satisfiesProperty IsPtr _ = False
  satisfiesProperty (HashProp p) (StakingHash cred) = satisfiesProperty p cred
  satisfiesProperty (HashProp _) _ = False

instance HasPermutationGenerator StakingCredentialProp StakingCredential where
  generators =
    let hash :: Abstraction CredentialProp Credential StakingCredentialProp StakingCredential =
          Abstraction
            { abstractionName = "hash"
            , propertyAbstraction = abstractsProperties HashProp
            , modelAbstraction = lens
              (\case
                StakingHash cred -> cred
                StakingPtr{} -> error "abstraction applied on bad data in StakingCredential permutation generator "
              )
              (\_ c -> StakingHash c)
            }
    in
    [ Morphism
      { name = "makePtr"
      , match = Yes
      , contract = clear >> add IsPtr
      , morphism = const $ StakingPtr <$> integer <*> integer <*> integer
      }
    , Morphism
      { name = "makeHash"
      , match = Yes
      , contract = clear >> addAll (HashProp <$> vars)
      , morphism = const $ StakingHash <$> genSatisfying (All $ Var <$> vars)
      }
    ] ++ (abstract hash <$> generators )
        where
          vars :: [CredentialProp]
          vars = M.keys $ M.filter id $ head $ solveAll logic

instance HasParameterisedGenerator StakingCredentialProp StakingCredential where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen StakingCredential
baseGen = pure $ StakingPtr 0 0 0

spec :: Spec
spec = do
    describe "StakingCredentialGenTests" $
      mapM_ fromHedgehogGroup $
        permutationGeneratorSelfTest
          True
          (\(_ :: Morphism StakingCredentialProp assetClassGenSelfTest) -> True)
          baseGen

