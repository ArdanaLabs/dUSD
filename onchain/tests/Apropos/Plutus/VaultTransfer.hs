module Apropos.Plutus.VaultTransfer
  ( VaultTransferProp(..)
  , VaultTransferModel(..)
  , spec
  ) where

import Gen (pubKeyHash)
import VaultTransfer
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Apropos
import Apropos.Script
import Apropos.ContextBuilder

data VaultTransferProp
    = Signed
    -- TODO model changes other than the config
  deriving stock (Eq,Ord,Show,Generic)
  deriving anyclass (Hashable,Enumerable)

newtype VaultTransferModel
  = VaultTransferModel
    {signatures :: [PubKeyHash]
    }
    deriving stock (Eq,Show)

magicpkh :: PubKeyHash
magicpkh = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

instance LogicalModel VaultTransferProp where
  logic = Yes

instance HasLogicalModel VaultTransferProp VaultTransferModel where
  satisfiesProperty Signed VaultTransferModel{signatures=sigs} = magicpkh `elem` sigs

instance HasPermutationGenerator VaultTransferProp VaultTransferModel where
  sources =
    [ Source
      { sourceName = "no signature"
      ,covers = Not $ Var Signed
      , gen = genFilter (not . satisfiesProperty Signed) $ VaultTransferModel <$> list (linear 0 10) pubKeyHash
      }
    ]

  generators =
    [ Morphism
      { name = "add signature"
      , match = Not $ Var Signed
      , contract = add Signed
      , morphism = \(VaultTransferModel sigs) -> pure $ VaultTransferModel (magicpkh : sigs)
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
              let VaultTransferModel sigs = m
              mapM_ addTxInfoSignatory sigs
     in applyMintingPolicyScript
          ctx
          vaultTransferPolicy
          (Redeemer $ BuiltinData $ toData ())

spec :: Spec
spec = do
  describe "nft tests" $ do
    fromHedgehogGroup $
      runGeneratorTestsWhere @VaultTransferProp "generator" Yes
    fromHedgehogGroup $
      permutationGeneratorSelfTest @VaultTransferProp
    fromHedgehogGroup $
      runScriptTestsWhere @VaultTransferProp "script tests" Yes
