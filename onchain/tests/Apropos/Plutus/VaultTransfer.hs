module Apropos.Plutus.VaultTransfer (
  VaultTransferProp (..),
  VaultTransferModel (..),
  spec,
) where

import Gen
import VaultTransfer

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

import Apropos
import Apropos.ContextBuilder
import Apropos.Script

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)

import Control.Monad (forM_)

data VaultTransferProp
  = Signed
  | ChangeDebt
  -- TODO model changes other than the config
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

-- This may or may not be a good way to do this?
-- if it is it can probably be generic
data VTxInInfo = VTxInInfo TxOutRef VTxOut
  deriving stock (Eq, Show)
data VTxOut = VTxOut Address Value (Maybe VaultDatumModel)
  deriving stock (Eq, Show)
data VaultDatumModel = VaultDatumModel Integer CurrencySymbol
  deriving stock (Eq, Show)

data VaultTransferModel = VaultTransferModel
  { signatures :: [PubKeyHash]
  , input :: VTxInInfo
  , output :: VTxOut
  }
  deriving stock (Eq, Show)

magicpkh :: PubKeyHash
magicpkh = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

instance LogicalModel VaultTransferProp where
  logic = Yes

instance HasLogicalModel VaultTransferProp VaultTransferModel where
  satisfiesProperty Signed VaultTransferModel {signatures = sigs} = magicpkh `elem` sigs
  satisfiesProperty ChangeDebt VaultTransferModel {input = i, output = o} =
    let VTxInInfo _ (VTxOut _ _ mdi) = i
        (VTxOut _ _ mdo) = o
     in case (mdi, mdo) of
          (Just (VaultDatumModel inDebt _), Just (VaultDatumModel outDebt _)) -> inDebt /= outDebt
          (Nothing, Nothing) -> False
          _ -> True

instance HasPermutationGenerator VaultTransferProp VaultTransferModel where
  sources =
    [ Source
        { sourceName = "no signature"
        , covers = Not (Var Signed) :&&: Not (Var ChangeDebt)
        , gen = do
            sigs <- list (linear 0 10) $ genFilter (/= magicpkh) pubKeyHash
            debt <- integer
            auth <- hexString @CurrencySymbol
            let inp =
                  VTxInInfo
                    (TxOutRef "" 0)
                    (VTxOut (Address (PubKeyCredential "") Nothing) mempty (Just (VaultDatumModel debt auth)))
            let out =
                  VTxOut (Address (PubKeyCredential "") Nothing) mempty (Just (VaultDatumModel debt auth))
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
    , Morphism
        { name = "change debt"
        , match = Not $ Var ChangeDebt
        , contract = add ChangeDebt
        , morphism = \m -> do
            let (VTxOut adr val mvd) = output m
            case mvd of
              Nothing -> error "vault had no datum?"
              Just (VaultDatumModel originalDebt auth) -> do
                newDebt <- genFilter (/= originalDebt) integer
                pure $ m {output = VTxOut adr val (Just $ VaultDatumModel newDebt auth)}
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