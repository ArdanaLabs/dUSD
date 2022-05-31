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
import Control.Arrow (second)
import Control.Monad.Identity (Identity)
import Control.Monad.State

import Plutarch.Api.V1 (PDatum (..))
import Plutarch.Builtin (pforgetData)
import Plutarch.Prelude

data VaultDepProp
  = AdaDecreased
  | DatumChanged
  | HasAuthToken
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data VaultDatum' = VaultDatum' Integer CurrencySymbol
  deriving stock (Eq, Show)

data VaultDepModel = VaultDepModel
  { input :: (Value, VaultDatum')
  , output :: (Value, Maybe VaultDatum')
  , minting :: Value
  }
  deriving stock (Eq, Show)

-- TODO I really don't like the way the formatter formats this tbh
toDatum :: VaultDatum' -> Datum
toDatum (VaultDatum' n cs) =
  plift $
    pcon . PDatum . pforgetData . pdata . pcon $
      VaultDatum $
        pdcons # pdata (pconstant n)
          #$ pdcons # pdata (pconstant cs) # pdnil

instance LogicalModel VaultDepProp where
  logic = Yes

instance HasLogicalModel VaultDepProp VaultDepModel where
  satisfiesProperty AdaDecreased m =
    let adaIn = assetClassValueOf (fst $ input m) ada
        adaOut = assetClassValueOf (fst $ output m) ada
     in adaIn > adaOut
  satisfiesProperty DatumChanged m =
    Just (snd (input m)) /= snd (output m)
  satisfiesProperty HasAuthToken m =
    let VaultDatum' _ cs = snd $ input m
     in assetClassValueOf (minting m) (Val.AssetClass (cs, "")) >= 1

genVaultDatum :: Gen VaultDatum'
genVaultDatum = VaultDatum' <$> pos <*> currencySymbol

instance HasPermutationGenerator VaultDepProp VaultDepModel where
  sources =
    [ Source
        { sourceName = "Valid but without auth"
        , covers = Not (Var AdaDecreased) :&&: Not (Var DatumChanged) :&&: Not (Var HasAuthToken)
        , gen = do
            v <- value
            vd@(VaultDatum' _ cs) <- genVaultDatum
            mintedJunk <-
              genFilter
                (\m -> assetClassValueOf m (Val.AssetClass (cs, "")) == 0)
                value
            pure $ VaultDepModel (v, vd) (v, Just vd) mintedJunk
        }
    ]
  generators =
    [ Morphism
        { name = "Change datum"
        , match = Not $ Var DatumChanged
        , contract = add DatumChanged
        , morphism = \m -> do
            nmd <- genFilter (/= Just (snd (input m))) $ maybeOf genVaultDatum
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
    , Morphism
        { name = "add auth token"
        , match = Not $ Var HasAuthToken
        , contract = add HasAuthToken
        , morphism = \m -> do
            let VaultDatum' _ cs = snd $ input m
            pure m {minting = minting m <> Val.singleton cs "" 1}
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
            uncurry (addInput (TxOutRef "" 0) mainAdr) $ second (Just . toDatum) (input m)
            uncurry (addOutput mainAdr) $ second (toDatum <$>) (output m)
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
