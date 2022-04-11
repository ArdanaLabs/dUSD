module Apropos.Plutus.SingletonValue (
  spec,
  SingletonValue,
  SingletonValueProp (..),
) where

import Apropos
import Apropos.Plutus.AssetClass (AssetClassProp)
import Apropos.Plutus.Integer (IntegerProp)
import Control.Lens
import Control.Monad (join)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value (AssetClass)

import Test.Syd
import Test.Syd.Hedgehog

type SingletonValue = (AssetClass, Integer)

data SingletonValueProp
  = AC AssetClassProp
  | Amt IntegerProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel SingletonValueProp where
  logic = (AC <$> logic) :&&: (Amt <$> logic)

instance HasLogicalModel SingletonValueProp SingletonValue where
  satisfiesProperty (AC p) (ac, _) = satisfiesProperty p ac
  satisfiesProperty (Amt p) (_, amt) = satisfiesProperty p amt

instance HasPermutationGenerator SingletonValueProp SingletonValue where
  generators =
    let l =
          ProductAbstraction
            { abstractionName = "assetClass"
            , propertyAbstraction = abstractsProperties AC
            , productModelAbstraction = _1
            }
        r =
          ProductAbstraction
            { abstractionName = "amt"
            , propertyAbstraction = abstractsProperties Amt
            , productModelAbstraction = _2
            }
     in join [abstract l <$> generators, abstract r <$> generators]

instance HasParameterisedGenerator SingletonValueProp SingletonValue where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen SingletonValue
baseGen =
  (,) <$> genSatisfying @AssetClassProp Yes
    <*> genSatisfying @IntegerProp Yes

spec :: Spec
spec = do
  describe "singletonValueGenSelfTests" $
    mapM_ fromHedgehogGroup $
      permutationGeneratorSelfTest
        True
        (\(_ :: Morphism SingletonValueProp singletonValueGenSelfTests) -> True)
        baseGen