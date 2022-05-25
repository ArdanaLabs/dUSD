{-# OPTIONS_GHC -wno-unused-imports #-}

module Apropos.Plutus.PriceModule (

) where

import Apropos
import Apropos.Script (ScriptModel(..))
import Control.Monad (replicateM)
import Data.List (uncons, length, drop, delete, find, findIndex, findIndices, (!!))
import Data.Maybe (mapMaybe)
import Data.Ratio
import GHC.Generics (Generic)
import Gen qualified
import Hedgehog.Gen   qualified as HG
import Hedgehog.Range qualified as HR
import Test.Syd hiding (Context)
import Test.Syd.Hedgehog

import Codec.Serialise (serialise)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (Blake2b_224 (Blake2b_224), HashAlgorithm)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import PlutusTx.Builtins qualified as Builtins

import Plutarch (compile, (#))
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude qualified as PPrelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval (always)
import Plutus.V1.Ledger.Scripts (Context(..), applyMintingPolicyScript)
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue, flattenValue, Value, assetClass, valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified

data PricePoint = PricePoint
  { ppTime :: POSIXTime
  , ppAdaPerUsd :: Rational  -- Maybe?
  , ppUsdPerAda :: Rational -- Maybe?
  } deriving stock (Show, Eq, Generic)

-- | The model for the properties.
data PriceModuleModel = PriceModuleModel
  { pmSignatures  :: [PubKeyHash]      -- Signatures present in the Tx.
  , pmPriceVectorIn  :: [PricePoint] -- ?
  , pmPriceVectorOut :: [PricePoint] -- ?
  , pmCurrency :: CurrencySymbol -- The currency symbol of the policy.
  , pmMinted :: Value      -- The value minted by the policy.
  , pmOwner  :: PubKeyHash -- The 'Owner' of this policy; baked into the policy.
  , pmInput  :: [TxInInfo] -- The inputs to the minting policy
  , pmOutput :: [TxOut]    -- The outputs of this policy.
  , pmAddress :: Address   -- Address the utxo is supposed to return to.
  , pmValidNFT :: AssetClass -- the NFT to show that the UTxO is correct.
  } deriving stock (Show, Eq, Generic)

data PriceModuleProp
  = BeenSigned
  | VectorHandled -- i.e. the price vector has been handled properly
  | VectorsSame -- For more control of morphisms
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance LogicalModel PriceModuleProp where
  logic = Var VectorsSame :->: VectorHandled
    --   :&&: (Var MintsCorrectly :->: Var MintsChoice) 
    --   :&&: (Var MintsChoice    :->: Var ValidCurChoice)


instance HasLogicalModel PriceModuleProp PriceModuleModel where
  satisfiesProperty BeenSigned modl = (mmOwner modl) `elem` (mmSignatures modl)
  satisfiesProperty VectorHandled modl { pmPriceVectorIn = pv1 , pmPriceVectorOut = pv2} =
    (pv1 == pv2) || (take 47 pv1) == (drop 1 pv2)
  satisfiesProperty VectorsSame modl { pmPriceVectorIn = pv1 , pmPriceVectorOut = pv2} =
    pv1 == pv2

instance HasPermutationGenerator ManagementProp ManagementModel where
  sources =
    [ Source
        { sourceName = "Correctly Formed"
        , covers = All 
          [ Var BeenSigned
          -- insert more
          ]
        , gen = do
             
            adr <- Gen.address
            -- Generating the Txs
            {-
            let inputTxOut :: TxOut
                inputTxOut = TxOut
                  { txOutAddress = adr
                  , txOutValue   = Value.singleton inNft "" 1 -- might need to add 2 ADA.
                  , txOutDatumHash = Nothing
                  }
            
            txRefId <- Gen.txId
            txRefIx <- fromIntegral <$> int (linear 0 3)

            let inputTxRef :: TxOutRef
                inputTxRef = TxOutRef
                  { txOutRefId  = txRefId
                  , txOutRefIdx = txRefIx
                  }
                
                inputTxIn :: TxInInfo
                inputTxIn = TxInInfo
                  { txInInfoOutRef   = inputTxRef
                  , txInInfoResolved = inputTxOut
                  }

            -- Constructing the model
            return $ PriceModuleModel
              { pm
              }
            -}
        }
    
    ]
  generators = 
    [ Morphism
        { name = "Unsign"
        , match = Var BeenSigned
        , contract = remove BeenSigned
        , morphism = \case
          modl@(ManagementModel {pmSignatures = sigs, pmOwner = owner}) -> do
            let sigs' = (delete owner sigs)
            return $ modl {pmSignatures = sigs'}
        }
    , Morphism
        { name = "Sign"
        , match = Not $ Var BeenSigned
        , contract = add BeenSigned
        , morphism = \case
          modl@(ManagementModel {pmSignatures = sigs, pmOwner = owner}) -> do
            let sigs' = (owner:sigs)
            return $ modl {pmSignatures = sigs'}
        }
    , Morphism
        { name = "AddPrice"
        , match = Yes
        , contract = branchIf VectorsSame (remove VectorsSame) (removeAll [VectorHandled, VectorsSame])
        , morphism = \case
          modl@(PriceModuleModel {pmPriceVectorOut = prices}) = do
            -- newPrice <- Gen.???
            newPrices = newPrice : (take 47 prices)
            return modl {pmPriceVectorOut = newPrices}
        }
    , Morphism
        { name = "ResetPrices"
        , match = Not $ Var VectorsSame
        , contract = addAll [VectorsSame, VectorHandled]
        , morphism = \case
          modl@(PriceModuleModel {pmPriceVectorIn = prices}) =
            return $ modl {pmPriceVectorOut = prices}
        }
    ]

instance HasParameterisedGenerator PriceModuleProp PriceModuleModel where
  parameterisedGenerator = buildGen

instance Enumerable PriceModuleProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel PriceModuleProp where
  logic = Var VectorsSame :->: VectorHandled
    -- (Var MintsCorrectly :->: Var MintsOne) 
    --   :&&: (Var MintsCorrectly :->: Var MintsChoice) 
    --   :&&: (Var MintsChoice    :->: Var ValidCurChoice)

instance ScriptModel ManagementProp ManagementModel where
  expect = (Var BeenSigned) 
             -- :&&: (Var InDatumHashed) 
             -- :&&: (Var OutDatumHashed)
             -- :&&: (Var ConfigPresent)
             -- :&&: (Var ConfigReturned)
             -- :&&: (Var OwnAtInBase)
             -- :&&: (Not (Var OwnAtOutBase ) :->: ( ??? ) -- i.e. if the CS changes.
  script mm = applyMintingPolicyScript (mkCtx mm) managementMintingPolicy (Redeemer (toBuiltinData ()))

priceModuleMintingPolicy :: MintingPolicy
priceModuleMintingPolicy = undefined -- TEMP

-- | Make the context from the Model.
mkCtx :: ManagementModel -> Context
mkCtx mm = Context $ PlutusTx.toBuiltinData scCtx
  where
    scCtx = ScriptContext txinf (Minting (mmOwnCurrency mm))
    txinf = TxInfo
{-
      { txInfoInputs  = mmInput mm
      , txInfoOutputs = mmOutput mm
      , txInfoFee = mempty
      , txInfoMint = mmMinted mm
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = always
      , txInfoSignatories = mmSignatures mm
      , txInfoData = [(mmInDatumHash mm,makeDatum (mmCurrencies mm)),(mmOutDatumHash mm,makeDatum (mmOutDatum mm))]
      , txInfoId = "" -- Temp?
      }
-}

-- | Safe version of `!!`.
indexVal :: [a] -> Int -> Maybe a
indexVal lst n = fst <$> uncons (drop n lst)

checkTxOut :: CurrencySymbol -> Integer -> Address -> DatumHash -> TxOut -> Bool
checkTxOut cs n adr dhsh txo =
  (txOutAddress txo == adr)
    && (txOutDatumHash txo == Just dhsh)
    && (n == valueOf (txOutValue txo) cs "")

-- TODO do this with a non-hack
-- (taken from HelloValidator)
datumHash :: Datum -> DatumHash
datumHash (Datum (BuiltinData d)) = (DatumHash . hashBlake2b_224 . Lazy.toStrict . serialise) d
  where
    _plutusHashWith :: HashAlgorithm alg => alg -> ByteString -> Builtins.BuiltinByteString
    _plutusHashWith alg = Builtins.toBuiltin . convert @_ @ByteString . hashWith alg
    hashBlake2b_224 :: ByteString -> Builtins.BuiltinByteString
    hashBlake2b_224 = _plutusHashWith Blake2b_224

makeDatum :: (PlutusTx.ToData a) => a -> Datum 
makeDatum x = Datum $ PlutusTx.toBuiltinData $ x

-- | Insert an element at position n of a list.
-- Note that if n > (length lst), the element
-- is inserted at the end.
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = (take n xs) ++ [x] ++ (drop n xs)
-- insertAt n x xs = let (!ys, !zs) = splitAt n xs in ys ++ [x] ++ zs
-- Tried various versions with Criterion;
-- went with most efficient version.

-- | Replace an element at position n of a list.
-- Note that if n > (length lst), the element
-- is inserted at the end, and nothing is replaced.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)
-- replaceAt n x xs = let (!ys, !zs) = splitAt n xs in ys ++ [x] ++ (drop 1 zs)

genNewPricePoint :: PricePoint -> Gen PricePoint
genNewPricePoint PricePoint {ppTime = tim, ppAdaPerUsd = apu, ppUsdPerAda = upa} = do
  mdf <- HG.double (HR.linearFracFrom 1 0.8 1.2)
  let orig = fromRational apu
      tempPrice' = orig * mdf
      tempPrice = approxRational tempPrice' 0.0001 -- maybe change epsilon?
  newPrice <- if (tempPrice /= 0)
      then return tempPrice
      else do
        mdf' <- HG.double (HR.linearFracFrom 1 1 1.2)
        let tempPrice2 = orig * mdf'
        return $ approxRational tempPrice2 0.0001
  dfTime <- HG.realFrac_ (HR.linearFracFrom 3600 300 18_000)
  let newTime = tim + dfTime
  return $ PricePoint {ppTime = newTime, ppAdaPerUsd = newPrice, ppUsdPerAda = recip newPrice}
-- Maybe generate a different modifier for USD/ADA?

-- note: Gen is just Hedgehog gen.
