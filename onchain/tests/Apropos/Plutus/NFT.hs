module Apropos.Plutus.NFT (
  NFTModel (..),
  NFTProp (..),
  spec,
) where

import Apropos
import Apropos.ContextBuilder
import Apropos.Script
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT)
import Gen
import NFT (nftMintingPolicy)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Scripts (applyMintingPolicyScript)
import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

newtype NFTModel = NFTModel
  -- TODO I think we should have data structures for these inlined datum types
  { inputs :: [(TxOutRef, Address, Value, Maybe Datum)]
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

isMagicInput :: (TxOutRef, a, b, c) -> Bool
isMagicInput (ref, _, _, _) = ref == inputRef

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
              list (linear 0 100) $
                (,,,)
                  <$> txOutRef
                  <*> address
                  <*> value
                  <*> maybeOf datum
        }
    ]
  generators =
    [ Morphism
        { name = "add right input"
        , match = Not $ Var SpendsRightInput
        , contract = add SpendsRightInput
        , morphism = \(NFTModel is) -> pure $ NFTModel $ (inputRef, Address (PubKeyCredential "") Nothing, mempty, Nothing) : is
        }
    , Morphism
        { name = "remove right input"
        , match = Var SpendsRightInput
        , contract = remove SpendsRightInput
        , morphism = \(NFTModel is) -> pure $ NFTModel $ filter (\(ref, _, _, _) -> ref /= inputRef) is
        }
    ]

instance HasParameterisedGenerator NFTProp NFTModel where
  parameterisedGenerator = buildGen

instance ScriptModel NFTProp NFTModel where
  expect = Yes

  -- TODO use real logic once validator is finished
  -- Var SpendsRightInput
  script m =
    let ctx = buildContext $ do
          withTxInfoBuilder @(StateT ScriptContext) @Identity @(StateT TxInfo) $ do
            sequence_ [addInput ref adr val md | let NFTModel is = m, (ref, adr, val, md) <- is]
     in applyMintingPolicyScript
          ctx
          (nftMintingPolicy inputRef)
          (Redeemer $ BuiltinData $ toData ())

spec :: Spec
spec =
  xdescribe "these pass but are pretty slow, I hope to look into this with fraser at some point" $
    describe "nft tests" $ do
      fromHedgehogGroup $
        runGeneratorTestsWhere @NFTProp "generator" Yes
      fromHedgehogGroup $
        permutationGeneratorSelfTest @NFTProp
      fromHedgehogGroup $
        runScriptTestsWhere @NFTProp "nft script tests" Yes
