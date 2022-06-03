module Apropos.Plutus.VaultTransfer (
  VaultTransferProp (..),
  VaultTransferModel (..),
  spec,
) where

import Gen (pubKeyHash)
import VaultTransfer

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Apropos
import Apropos.ContextBuilder
import Apropos.Script

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)

import Control.Monad (forM_)
import Data.Maybe (isJust)

data VaultTransferProp
  = Signed
  | ChangesDatum
  -- TODO model changes other than the config
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

data VaultTransferModel = VaultTransferModel
  { signatures :: [PubKeyHash]
  , input :: TxInInfo'
  , output :: TxInInfo'
  }
  deriving stock (Eq, Show)

magicpkh :: PubKeyHash
magicpkh = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

instance LogicalModel VaultTransferProp where
  logic = Yes

instance HasLogicalModel VaultTransferProp VaultTransferModel where
  satisfiesProperty Signed VaultTransferModel {signatures = sigs} = magicpkh `elem` sigs
  satisfiesProperty ChangesDatum VaultTransferModel {input = i, output = o} =
    let TxInInfo' _ (TxOut' _ _ mdi) = i
        TxInInfo' _ (TxOut' _ _ mdo) = o
     in case (mdi, mdo) of
          (Just (Datum (BuiltinData inpd)), Just (Datum (BuiltinData outd))) -> isJust $ do
            -- this is a dummy type to compile for now
            _ <- fromData @Integer inpd
            _ <- undefined outd
            undefined
          (Nothing, Nothing) -> False
          _ -> True

instance HasPermutationGenerator VaultTransferProp VaultTransferModel where
  sources =
    [ Source
        { sourceName = "no signature"
        , covers = Not $ Var Signed
        , gen = do
            sigs <- list (linear 0 10) $ genFilter (/= magicpkh) pubKeyHash
            let inp =
                  TxInInfo'
                    (TxOutRef "" 0)
                    (TxOut' (Address (PubKeyCredential "") Nothing) mempty Nothing)
            let out =
                  TxInInfo'
                    (TxOutRef "" 0)
                    (TxOut' (Address (PubKeyCredential "") Nothing) mempty Nothing)

            pure $ VaultTransferModel sigs inp out
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
              forM_ (signatures m) addTxInfoSignatory
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
