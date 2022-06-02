module Apropos.Plutus.VaultTransfer (
  VaultTransferProp (..),
  VaultTransferModel (..),
  spec,
) where

import Gen (pubKeyHash)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)
import VaultTransfer

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Apropos
import Apropos.ContextBuilder
import Apropos.Script

data VaultTransferProp
  = Signed
  -- TODO model changes other than the config
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

data VaultTransferModel = VaultTransferModel
  { signatures :: [PubKeyHash]
  , input :: TxInInfo
  }
  deriving stock (Eq, Show)

magicpkh :: PubKeyHash
magicpkh = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

instance LogicalModel VaultTransferProp where
  logic = Yes

instance HasLogicalModel VaultTransferProp VaultTransferModel where
  satisfiesProperty Signed VaultTransferModel {signatures = sigs} = magicpkh `elem` sigs

instance HasPermutationGenerator VaultTransferProp VaultTransferModel where
  sources =
    [ Source
        { sourceName = "no signature"
        , covers = Not $ Var Signed
        , gen = do
            sigs <- list (linear 0 10) $ genFilter (/= magicpkh) pubKeyHash
            let inp =
                  TxInInfo
                    (TxOutRef "" 0)
                    (TxOut (Address (PubKeyCredential "") Nothing) mempty Nothing)
            pure $ VaultTransferModel sigs inp
        }
    ]

  generators =
    [ Morphism
        { name = "add signature"
        , match = Not $ Var Signed
        , contract = add Signed
        , morphism = \m -> pure $ m {signatures = magicpkh : signatures m}
        }
    ]

instance HasParameterisedGenerator VaultTransferProp VaultTransferModel where
  parameterisedGenerator = buildGen

instance ScriptModel VaultTransferProp VaultTransferModel where
  expect = Yes -- Var Signed
  script m =
    let ctx =
          buildContext $ do
            withTxInfo $ do
              mapM_ addTxInfoSignatory (signatures m)
     in applyMintingPolicyScript
          ctx
          vaultTransferPolicy
          (Redeemer $ BuiltinData $ toData ())

spec :: Spec
spec = do
  describe "vault transferr tests" $ do
    fromHedgehogGroup $
      runGeneratorTestsWhere @VaultTransferProp "generator" Yes
    fromHedgehogGroup $
      permutationGeneratorSelfTest @VaultTransferProp
    fromHedgehogGroup $
      runScriptTestsWhere @VaultTransferProp "script tests" Yes
