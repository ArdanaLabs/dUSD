module Apropos.Plutus.NFT (
  NFTModel (..),
  NFTProp (..),
  spec,
) where

import Apropos
import Apropos.Script
import Control.Monad (replicateM)
import Data.String (IsString (fromString))
import NFT (asCurrencySymbol, asPlutarch)
import Plutarch
import Plutarch.Builtin (pforgetData)
import Plutarch.Prelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value qualified as V
import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

newtype NFTModel = NFTModel
  { inputs :: [TxInInfo]
  }
  deriving stock (Show)

data NFTProp
  = SpendsRightInput
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

instance LogicalModel NFTProp where
  logic = Yes

instance HasLogicalModel NFTProp NFTModel where
  satisfiesProperty SpendsRightInput m = any isMagicInput $ inputs m

isMagicInput :: TxInInfo -> Bool
isMagicInput (TxInInfo (TxOutRef txid _) _) = txid == inputId

inputId :: TxId
inputId = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

instance HasPermutationGenerator NFTProp NFTModel where
  sources =
    [ Source
        { sourceName = "junk"
        , covers = Yes
        , gen = NFTModel <$> list (linear 0 100) genTxinfo
        }
    ]
  generators =
    [ Morphism
        { name = "add right input"
        , match = Not $ Var SpendsRightInput
        , contract = add SpendsRightInput
        , morphism = \(NFTModel is) -> pure $ NFTModel (TxInInfo (TxOutRef inputId 0) (TxOut (Address (PubKeyCredential "") Nothing) mempty Nothing) : is)
        }
    , Morphism
        { name = "remove right input"
        , match = Var SpendsRightInput
        , contract = remove SpendsRightInput
        , morphism = \(NFTModel is) -> pure $ NFTModel $ filter (\(TxInInfo (TxOutRef txid _) _) -> txid /= inputId) is
        }
    ]

instance HasParameterisedGenerator NFTProp NFTModel where
  parameterisedGenerator = buildGen

instance ScriptModel NFTProp NFTModel where
  script m = compile $ asPlutarch inputId # pforgetData (pdata (pconstant ())) # pconstant (scFrom m)
  expect = Var SpendsRightInput

scFrom :: NFTModel -> ScriptContext
scFrom m =
  ScriptContext
    ( TxInfo
        (inputs m)
        []
        mempty
        mempty
        []
        []
        (Interval (LowerBound NegInf False) (UpperBound PosInf False))
        []
        []
        (TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    )
    (Minting (asCurrencySymbol inputId))

spec :: Spec
spec =
  xdescribe "nft tests" $ do
    fromHedgehogGroup $
      runGeneratorTestsWhere @NFTProp "generator" Yes
    fromHedgehogGroup $
      permutationGeneratorSelfTest @NFTProp
    xdescribe "postponed only because the nft policy isn't implemented yet" $
      fromHedgehogGroup $
        runScriptTestsWhere @NFTProp "nft script tests" Yes

genTxinfo :: Gen TxInInfo
genTxinfo =
  TxInInfo
    <$> (TxOutRef <$> genTxId <*> genInt)
    <*> (TxOut <$> genAdr <*> genValue <*> pure Nothing)

genInt :: Gen Integer
genInt = fromIntegral <$> int (linear 0 1_000_000)

genValue :: Gen Value
genValue = mconcat <$> list (linear 0 10) genSingletonValue
  where
    genSingletonValue :: Gen Value
    genSingletonValue = V.singleton <$> genHexString <*> genHexString <*> genInt

genTxId :: Gen TxId
genTxId = TxId <$> genHexString

genAdr :: Gen Address
genAdr = Address <$> genCredential <*> pure Nothing

genCredential :: Gen Credential
genCredential = PubKeyCredential <$> genHexString

genHexString :: IsString s => Gen s
genHexString = fromString <$> replicateM 64 (element "01234567890abcdef")
