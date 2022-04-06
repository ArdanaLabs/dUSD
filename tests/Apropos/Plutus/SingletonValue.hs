module Apropos.Plutus.SingletonValue (
  spec,
  SingletonValue,
  SingletonValueProp (..),
) where

import Apropos
import Apropos.Plutus.AssetClass (AssetClassProp)
import Apropos.Plutus.Integer (IntegerProp)

import Control.Lens (Field1 (_1), Field2 (_2))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value (AssetClass)

import Test.Syd
import Test.Syd.Hedgehog

type SingletonValue = (AssetClass, Integer)

data SingletonValueProp
  = AC AssetClassProp
  | Amt IntegerProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel SingletonValueProp where
  logic = abstractionLogic @SingletonValue

instance HasLogicalModel SingletonValueProp SingletonValue where
  satisfiesProperty (AC p) (ac, _) = satisfiesProperty p ac
  satisfiesProperty (Amt p) (_, amt) = satisfiesProperty p amt

instance HasAbstractions SingletonValueProp SingletonValue where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "assetClass"
          , propertyAbstraction = abstractsProperties AC
          , productModelAbstraction = _1
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "amt"
          , propertyAbstraction = abstractsProperties Amt
          , productModelAbstraction = _2
          }
    ]

instance HasPermutationGenerator SingletonValueProp SingletonValue where
  generators = abstractionMorphisms

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
