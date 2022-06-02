module Apropos.Plutus.NFT (
  NFTModel (..),
  NFTProp (..),
  spec,
) where

import Apropos
import Apropos.ContextBuilder
import Apropos.Script
import Gen
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)

import NFT (nftMintingPolicy)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

newtype NFTModel = NFTModel
  -- TODO I think we should have data structures for these inlined datum types
  { inputs :: [TxInInfo']
  }
  deriving stock (Show, Eq)

data NFTProp
  = SpendsRightInput
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

instance LogicalModel NFTProp where
  logic = Yes

instance HasLogicalModel NFTProp NFTModel where
  satisfiesProperty SpendsRightInput m = any isMagicInput $ inputs m

isMagicInput :: TxInInfo' -> Bool
isMagicInput (TxInInfo' ref _) = ref == inputRef

inputRef :: TxOutRef
inputRef =
  TxOutRef
    "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    0

instance HasPermutationGenerator NFTProp NFTModel where
  sources =
    [ Source
        { sourceName = "junk"
        , covers = Yes
        , gen =
            (NFTModel <$>) $
              list (linear 0 1) $
                TxInInfo'
                  <$> txOutRef
                  <*> ( TxOut'
                          <$> address
                          <*> value
                          <*> maybeOf datum
                      )
        }
    ]
  generators =
    [ Morphism
        { name = "add right input"
        , match = Not $ Var SpendsRightInput
        , contract = add SpendsRightInput
        , morphism = \(NFTModel is) ->
            pure $ NFTModel $ TxInInfo' inputRef (TxOut' (Address (PubKeyCredential "") Nothing) mempty Nothing) : is
        }
    , Morphism
        { name = "remove right input"
        , match = Var SpendsRightInput
        , contract = remove SpendsRightInput
        , morphism = \(NFTModel is) ->
            pure $ NFTModel $ filter (\(TxInInfo' ref _) -> ref /= inputRef) is
        }
    ]

instance HasParameterisedGenerator NFTProp NFTModel where
  parameterisedGenerator = buildGen

instance ScriptModel NFTProp NFTModel where
  -- TODO use real logic once validator is finished
  -- Var SpendsRightInput
  expect = Yes

  script m =
    let ctx = buildContext $ do
          withTxInfo $ do
            mapM_ addInput (inputs m)
     in applyMintingPolicyScript
          ctx
          (nftMintingPolicy inputRef)
          (Redeemer $ BuiltinData $ toData ())

spec :: Spec
spec =
  describe "nft tests" $ do
    fromHedgehogGroup $
      runGeneratorTestsWhere @NFTProp "generator" Yes
    fromHedgehogGroup $
      permutationGeneratorSelfTest @NFTProp
    fromHedgehogGroup $
      runScriptTestsWhere @NFTProp "nft script tests" Yes
