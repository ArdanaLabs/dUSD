module Apropos.Plutus.VaultDeposit (
  VaultDepProp (..),
  VaultDepModel (..),
  spec,
) where

import Apropos
import Apropos.Plutus.AssetClass (ada)
import Apropos.Script

import Gen
import VaultDeposit

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)
import Plutus.V1.Ledger.Value (assetClassValueOf)
import Plutus.V1.Ledger.Value qualified as Val

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Apropos.ContextBuilder
import Control.Monad.Identity (Identity)
import Control.Monad.State

data VaultDepProp
  = AdaDecreased
  | DatumChanged
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data VaultDepModel = VaultDepModel
  { input :: (Value, Maybe Datum)
  , output :: (Value, Maybe Datum)
  }
  deriving stock (Eq, Show)

instance LogicalModel VaultDepProp where
  logic = Yes

instance HasLogicalModel VaultDepProp VaultDepModel where
  satisfiesProperty AdaDecreased m =
    let adaIn = assetClassValueOf (fst $ input m) ada
        adaOut = assetClassValueOf (fst $ output m) ada
     in adaIn > adaOut
  satisfiesProperty DatumChanged m =
    snd (input m) /= snd (output m)

instance HasPermutationGenerator VaultDepProp VaultDepModel where
  sources =
    [ Source
        { sourceName = "Valid"
        , covers = Not (Var AdaDecreased) :&&: Not (Var DatumChanged)
        , gen = do
            v <- value
            md <- maybeOf datum
            pure $ VaultDepModel (v, md) (v, md)
        }
    ]
  generators =
    [ Morphism
        { name = "Change datum"
        , match = Not $ Var DatumChanged
        , contract = add DatumChanged
        , morphism = \m -> do
            nmd <- genFilter (/= snd (input m)) $ maybeOf datum
            pure $ m {output = (fst $ output m, nmd)}
        }
    , Morphism
        { name = "decrease ada"
        , match = Not $ Var AdaDecreased
        , contract = add AdaDecreased
        , morphism = \m -> do
            -- it's easier to just add to the input because if the input was 0 you can't decrease it
            addAdaInput <- pos
            pure $ m {input = (fst (input m) <> Val.singleton adaSymbol adaToken addAdaInput, snd $ input m)}
        }
    ]

instance HasParameterisedGenerator VaultDepProp VaultDepModel where
  parameterisedGenerator = buildGen

instance ScriptModel VaultDepProp VaultDepModel where
  expect = Yes

  -- Not (Var AdaDecreased) :&&: Not (Var DatumChanged)
  -- TODO use commented correct logic
  script m =
    let ctx = buildContext $ do
          withTxInfoBuilder @(StateT ScriptContext) @Identity @(StateT TxInfo) $ do
            uncurry (addInput (TxOutRef "" 0) mainAdr) $ input m
            uncurry (addOutput mainAdr) $ output m
     in applyMintingPolicyScript
          ctx
          vaultDepositPolicy
          (Redeemer $ BuiltinData $ toData ())

mainAdr :: Address
mainAdr = Address (ScriptCredential "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") Nothing

-- TODO this should have the hash of the real validator

spec :: Spec
spec = do
  describe "vault deposit" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere @VaultDepProp "gen" Yes
    fromHedgehogGroup $ runScriptTestsWhere @VaultDepProp "script" Yes
