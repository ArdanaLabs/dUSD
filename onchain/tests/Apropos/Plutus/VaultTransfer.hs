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
import Plutus.V1.Ledger.Value qualified as Val

import Plutarch.Builtin
import Plutarch.Prelude
import Types

data VaultTransferProp
  = MintsAuthCS
  | ChangeDebt
  | MoveAdr
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

-- TODO
-- I think this is a fairly good way to go
-- but it would be better with some sort of GADT
-- and a type class that converts it to a plutarch type with a pdata instance
data VTxInInfo = VTxInInfo TxOutRef VTxOut
  deriving stock (Eq, Show)
data VTxOut = VTxOut Address Value (Maybe VaultDatumModel)
  deriving stock (Eq, Show)
data VaultDatumModel = VaultDatumModel Integer CurrencySymbol
  deriving stock (Eq, Show)

marshalToData :: VaultDatumModel -> Data
marshalToData (VaultDatumModel debt auth) =
  plift $
    pforgetData . pdata . pcon . VaultDatum $
      pdcons # pdata (pconstant debt)
        #$ pdcons # pdata (pconstant auth)
        #$ pdnil

toTxOut' :: VTxOut -> TxOut'
toTxOut' (VTxOut a v md) = TxOut' a v (Datum . BuiltinData . marshalToData <$> md)

toTxInInfo' :: VTxInInfo -> TxInInfo'
toTxInInfo' (VTxInInfo ref out) = TxInInfo' ref (toTxOut' out)

-- TODO
-- we should probably model extra other outputs and inputs
data VaultTransferModel = VaultTransferModel
  { minting :: Value
  , input :: VTxInInfo
  , output :: VTxOut
  }
  deriving stock (Eq, Show)

instance LogicalModel VaultTransferProp where
  logic = Yes

instance HasLogicalModel VaultTransferProp VaultTransferModel where
  satisfiesProperty ChangeDebt VaultTransferModel {input = i, output = o} =
    let VTxInInfo _ (VTxOut _ _ mdi) = i
        VTxOut _ _ mdo = o
     in case (mdi, mdo) of
          (Just (VaultDatumModel inDebt _), Just (VaultDatumModel outDebt _)) ->
            inDebt /= outDebt
          (Nothing, Nothing) -> False
          _ -> True
  satisfiesProperty
    MoveAdr
    VaultTransferModel
      { input = VTxInInfo _ (VTxOut inAdr _ _)
      , output = VTxOut outAdr _ _
      } =
      inAdr /= outAdr
  satisfiesProperty MintsAuthCS VaultTransferModel {minting = m, input = VTxInInfo _ (VTxOut _ _ mvd)} =
    case mvd of
      Nothing -> False
      Just (VaultDatumModel _ auth) ->
        Val.assetClassValueOf m (Val.AssetClass (auth, "")) > 0

instance HasPermutationGenerator VaultTransferProp VaultTransferModel where
  sources =
    [ Source
        { sourceName = "no signature"
        , covers = None $ Var <$> [ChangeDebt, MoveAdr, MintsAuthCS]
        , gen = do
            debt <- integer
            auth <- hexString @CurrencySymbol
            let inp =
                  VTxInInfo
                    (TxOutRef "" 0)
                    ( VTxOut
                        (Address (PubKeyCredential "") Nothing)
                        mempty
                        (Just (VaultDatumModel debt auth))
                    )
            let out =
                  VTxOut (Address (PubKeyCredential "") Nothing) mempty (Just (VaultDatumModel debt auth))
            pure $ VaultTransferModel mempty inp out
        }
    ]

  generators =
    [ Morphism
        { name = "change debt"
        , match = Not $ Var ChangeDebt
        , contract = add ChangeDebt
        , morphism = \m -> do
            let (VTxOut adr val mvd) = output m
            case mvd of
              Nothing -> error "vault had no datum?"
              Just (VaultDatumModel originalDebt auth) -> do
                newDebt <- genFilter (/= originalDebt) integer
                pure $
                  m
                    { output = VTxOut adr val (Just $ VaultDatumModel newDebt auth)
                    }
        }
    , Morphism
        { name = "move adr"
        , match = Not $ Var MoveAdr
        , contract = add MoveAdr
        , morphism = \m -> do
            let VTxOut oldadr v md = output m
            adr <- genFilter (/= oldadr) address
            pure $ m {output = VTxOut adr v md}
        }
    , Morphism
        { name = "mint authCS"
        , match = Not $ Var MintsAuthCS
        , contract = add MintsAuthCS
        , morphism = \m -> do
            let VTxInInfo _ (VTxOut _ _ mvd) = input m
            case mvd of
              Nothing -> error "vault had no datum?"
              Just (VaultDatumModel _originalDebt auth) -> do
                pure m {minting = minting m <> Val.singleton auth "" 1}
        }
    ]

instance HasParameterisedGenerator VaultTransferProp VaultTransferModel where
  parameterisedGenerator = buildGen

instance ScriptModel VaultTransferProp VaultTransferModel where
  expect = (Yes :||:) $ Var MintsAuthCS :&&: Not (Var ChangeDebt) :&&: Not (Var MoveAdr)
  script m =
    let ctx =
          buildContext $ do
            withTxInfo $ do
              addInput $ toTxInInfo' (input m)
              addOutput $ toTxOut' (output m)
              mint $ minting m
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
