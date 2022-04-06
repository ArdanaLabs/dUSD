module Apropos.Plutus.Vault (
  VaultProp,
  spec,
  makeVaultDatum,
  makeVaultTxout,
) where

import Apropos

import Apropos.Plutus.Address (AddressProp (..))
import Apropos.Plutus.AssetClass (AssetClassProp (..))
import Apropos.Plutus.Integer (IntegerProp (..))
import Apropos.Plutus.SingletonValue (SingletonValue, SingletonValueProp (..))

import Control.Lens (lens)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Plutus.V1.Ledger.Api (
  Address,
  Datum (..),
  DatumHash (..),
  TxOut (..),
 )
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Plutarch (PCon (pcon), (#))
import Plutarch.Api.V1 (PDatum (PDatum))
import Plutarch.Builtin (
  PIsData (pdata),
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Lift (pconstant, plift)

spec :: Spec
spec = do
  describe "vault model" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: VaultModel :+ VaultProp) "generator" Yes

data VaultModel = VaultModel
  { collateral :: SingletonValue
  , debt :: SingletonValue
  , addr :: Address
  }
  deriving stock (Eq, Show)

data VaultProp
  = DebtProp SingletonValueProp
  | CollateralProp SingletonValueProp
  | Addr AddressProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel VaultProp where
  logic =
    abstractionLogic @VaultModel
      -- logic for performance
      :&&: Var (CollateralProp (AC IsAda))
      :&&: Var (DebtProp (AC IsDUSD))
      :&&: Not (Var (CollateralProp (Amt IsNegative)))
      :&&: Not (Var (DebtProp (Amt IsNegative)))

instance HasLogicalModel VaultProp VaultModel where
  satisfiesProperty (DebtProp p) vm = satisfiesProperty p (debt vm)
  satisfiesProperty (CollateralProp p) vm = satisfiesProperty p (collateral vm)
  satisfiesProperty (Addr p) vm = satisfiesProperty p (addr vm)

instance HasAbstractions VaultProp VaultModel where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "debt"
          , propertyAbstraction = abstractsProperties DebtProp
          , productModelAbstraction = lens debt (\vm debt' -> vm {debt = debt'})
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "collateral"
          , propertyAbstraction = abstractsProperties CollateralProp
          , productModelAbstraction = lens collateral (\vm collateral' -> vm {collateral = collateral'})
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "address"
          , propertyAbstraction = abstractsProperties Addr
          , productModelAbstraction = lens addr (\vm addr' -> vm {addr = addr'})
          }
    ]

instance HasPermutationGenerator VaultProp VaultModel where
  generators = abstractionMorphisms

instance HasParameterisedGenerator VaultProp VaultModel where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen VaultModel
baseGen =
  VaultModel
    <$> genSatisfying (Not (Var (Amt IsNegative)) :&&: Var (AC IsAda))
    <*> genSatisfying (Not (Var (Amt IsNegative)) :&&: Var (AC IsDUSD))
    <*> genSatisfying @AddressProp Yes

makeVaultTxout :: VaultModel -> (TxOut, (Datum, DatumHash))
makeVaultTxout vm =
  let d = makeVaultDatum vm
      dh = hashDatum d
      bal = uncurry assetClassValue (collateral vm)
   in (TxOut (addr vm) bal (Just dh), (d, dh))

-- TODO real hashes
hashDatum :: Datum -> DatumHash
hashDatum _ = "aa"

makeVaultDatum :: VaultModel -> Datum
makeVaultDatum VaultModel {collateral = (AssetClass (collateralCs, collateralTn), collateralAmt), debt = (AssetClass (debtCs, debtTn), debtAmt)} =
  -- TODO we may want to change the way the vault datum is encoded
  plift $
    pcon $
      PDatum $
        pforgetData $
          pdata $
            ppairDataBuiltin
              # pdata
                ( ppairDataBuiltin
                    # pdata (ppairDataBuiltin # pdata (pconstant collateralCs) # pdata (pconstant collateralTn))
                    # pdata (pconstant collateralAmt)
                )
              # pdata
                ( ppairDataBuiltin
                    # pdata (ppairDataBuiltin # pdata (pconstant debtCs) # pdata (pconstant debtTn))
                    # pdata (pconstant debtAmt)
                )
