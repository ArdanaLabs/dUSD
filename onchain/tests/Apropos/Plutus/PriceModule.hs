{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Apropos.Plutus.PriceModule (
  ) where

import Apropos
import Apropos.Script (ScriptModel (..))
import Control.Monad (liftM, replicateM)
import Data.List (delete, drop, find, findIndex, findIndices, length, uncons, (!!))
import Data.Maybe (mapMaybe)
import Data.Ratio
import GHC.Generics (Generic)
import Gen qualified
import Hedgehog.Gen qualified as HG
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
import Plutus.V1.Ledger.Scripts (Context (..), applyMintingPolicyScript)
import Plutus.V1.Ledger.Value (AssetClass, Value, assetClass, assetClassValue, assetClassValueOf, flattenValue, valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Ratio qualified as Rat

data PricePoint = PricePoint
  { ppTime :: POSIXTime
  , ppAdaPerUsd :: Rational -- Maybe?
  , ppUsdPerAda :: Rational -- Maybe?
  }
  deriving stock (Show, Eq, Generic)

data PricePoint' = PricePoint'
  { ppTime' :: POSIXTime
  , ppAdaPerUsd' :: Rat.Rational -- Maybe?
  , ppUsdPerAda' :: Rat.Rational -- Maybe?
  }
  deriving stock (Show, Eq, Generic)

PlutusTx.unstableMakeIsData ''PricePoint'

ppToTx :: PricePoint -> PricePoint'
ppToTx PricePoint {ppTime = tim, ppAdaPerUsd = apu, ppUsdPerAda = upa} =
  PricePoint' (tim) (Rat.fromGHC apu) (Rat.fromGHC upa)

txToPp :: PricePoint' -> PricePoint
txToPp PricePoint' {ppTime' = tim, ppAdaPerUsd' = apu, ppUsdPerAda' = upa} =
  PricePoint (tim) (Rat.toGHC apu) (Rat.toGHC upa)

instance ToData PricePoint where
  toBuiltinData pp = toBuiltinData (ppToTx pp)

instance FromData PricePoint where
  fromBuiltinData pp = txToPp <$> fromBuiltinData pp

-- | The model for the properties.
data PriceModuleModel = PriceModuleModel
  { pmSignatures :: [PubKeyHash] -- Signatures present in the Tx.
  , pmPriceVectorIn :: [PricePoint] -- PriceVector Input
  , pmPriceVectorOut :: [PricePoint] -- PriceVector Output
  , pmCurrency :: CurrencySymbol -- The currency symbol of the policy.
  , pmMinted :: Value -- The value minted by the policy.
  , pmOwner :: PubKeyHash -- The 'Owner' of this policy; baked into the policy.
  , pmInput :: [TxInInfo] -- The inputs to the minting policy
  , pmOutput :: [TxOut] -- The outputs of this policy.
  , pmAddress :: Address -- Address the utxo is supposed to return to.
  , pmValidNFT :: AssetClass -- the NFT to show that the UTxO is correct.
  }
  deriving stock (Show, Eq, Generic)

pmPVIHash :: PriceModuleModel -> DatumHash
pmPVIHash = datumHash . makeDatum . pmPriceVectorIn

pmPVOHash :: PriceModuleModel -> DatumHash
pmPVOHash = datumHash . makeDatum . pmPriceVectorOut

data PriceModuleProp
  = BeenSigned
  | VectorHandled -- i.e. the price vector has been handled properly
  | VectorsSame -- For more control of morphisms
  | MintsToken -- Mints the token.
  | InputHasHash -- Input  has the hash of pmPriceVectorIn
  | OutputHasHash -- Output has the hash of pmPriceVectorOut
  | InputHasNFT
  | OutputHasNFT
  | InputHasAdr
  | OutputHasAdr
  | InputHasAll
  | OutputHasAll
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance LogicalModel PriceModuleProp where
  logic =
    (Var VectorsSame :->: Var VectorHandled)
      :&&: (Var InputHasAll :->: (Var InputHasHash :&&: Var InputHasNFT :&&: Var InputHasAdr))
      :&&: (Var OutputHasAll :->: (Var OutputHasHash :&&: Var OutputHasNFT :&&: Var OutputHasAdr))

--   :&&: (Var MintsCorrectly :->: Var MintsChoice)
--   :&&: (Var MintsChoice    :->: Var ValidCurChoice)

instance HasLogicalModel PriceModuleProp PriceModuleModel where
  satisfiesProperty BeenSigned modl = (pmOwner modl) `elem` (pmSignatures modl)
  satisfiesProperty VectorHandled PriceModuleModel {pmPriceVectorIn = pv1, pmPriceVectorOut = pv2} =
    (pv1 == pv2) || (take 47 pv1) == (drop 1 pv2)
  satisfiesProperty VectorsSame PriceModuleModel {pmPriceVectorIn = pv1, pmPriceVectorOut = pv2} =
    pv1 == pv2
  -- May have to change this to ensure that no other tokens are minted
  -- by other minting policies, depending on the requirements.
  satisfiesProperty MintsToken PriceModuleModel {pmMinted = mint, pmValidNFT = nft} =
    assetClassValueOf mint nft == 1
  satisfiesProperty InputHasHash modl@(PriceModuleModel {pmInput = inp}) =
    let dhsh = pmPVIHash modl
     in any (checkTxOutHash dhsh) (map txInInfoResolved inp)
  satisfiesProperty OutputHasHash modl@(PriceModuleModel {pmOutput = outp}) =
    let dhsh = pmPVOHash modl
     in any (checkTxOutHash dhsh) outp
  -- Maybe for the next few, change 'any' to 'exactly 1'.
  satisfiesProperty InputHasNFT (PriceModuleModel {pmInput = inp, pmValidNFT = ac}) =
    any (checkTxOutVal ac 1) (map txInInfoResolved inp)
  satisfiesProperty OutputHasNFT (PriceModuleModel {pmOutput = outp, pmValidNFT = ac}) =
    any (checkTxOutVal ac 1) outp
  satisfiesProperty InputHasAdr (PriceModuleModel {pmInput = inp, pmAddress = adr}) =
    any (checkTxOutAdr adr) (map txInInfoResolved inp)
  satisfiesProperty OutputHasAdr (PriceModuleModel {pmOutput = outp, pmAddress = adr}) =
    any (checkTxOutAdr adr) outp
  satisfiesProperty InputHasAll modl@(PriceModuleModel {pmInput = inp, pmAddress = adr, pmValidNFT = ac}) =
    let dhsh = pmPVIHash modl
     in any (checkTxOutAC ac 1 adr dhsh) (map txInInfoResolved inp)
  satisfiesProperty OutputHasAll modl@(PriceModuleModel {pmOutput = outp, pmAddress = adr, pmValidNFT = ac}) =
    let dhsh = pmPVOHash modl
     in any (checkTxOutAC ac 1 adr dhsh) outp

-- checkTxOutAC ac n adr dhsh txo =

instance HasPermutationGenerator PriceModuleProp PriceModuleModel where
  sources =
    [ Source
        { sourceName = "Correctly Formed"
        , covers =
            All
              [ Var BeenSigned
              , Var VectorHandled
              , Var VectorsSame
              , Var InputHasHash
              , Var OutputHasHash
              ]
        , gen = do
            adr <- Gen.address
            let seedPoint = PricePoint 1653506000 1 1
            numPoints <- int (linear 30 80)
            pricePts <- take 48 . reverse <$> iterateM numPoints genNewPricePoint seedPoint

            -- Signatures
            ownPkh <- Gen.pubKeyHash
            numSigs <- int (linear 0 4)
            sigs <- (ownPkh :) <$> replicateM numSigs Gen.pubKeyHash

            -- Currencies
            ownCS <- Gen.hexString @CurrencySymbol
            inNFC <- Gen.hexString @CurrencySymbol
            inNFT <- Gen.tokenName

            -- Generating the Txs
            let inputTxOut :: TxOut
                inputTxOut =
                  TxOut
                    { txOutAddress = adr
                    , txOutValue = Value.singleton inNFC inNFT 1 -- might need to add 2 ADA.
                    , txOutDatumHash = Just $ datumHash $ makeDatum pricePts
                    }

            txRefId <- Gen.txId
            txRefIx <- fromIntegral <$> int (linear 0 3)

            let inputTxRef :: TxOutRef
                inputTxRef =
                  TxOutRef
                    { txOutRefId = txRefId
                    , txOutRefIdx = txRefIx
                    }

                inputTxIn :: TxInInfo
                inputTxIn =
                  TxInInfo
                    { txInInfoOutRef = inputTxRef
                    , txInInfoResolved = inputTxOut
                    }

            -- Constructing the model
            return $
              PriceModuleModel
                { pmSignatures = sigs
                , pmPriceVectorIn = pricePts
                , pmPriceVectorOut = pricePts
                , pmCurrency = ownCS
                , pmMinted = Value.singleton ownCS "" 1
                , pmOwner = ownPkh
                , pmInput = [inputTxIn]
                , pmOutput = [inputTxOut]
                , pmAddress = adr
                , pmValidNFT = assetClass inNFC inNFT
                }
        }
    ]
  generators =
    [ Morphism
        { name = "Unsign"
        , match = Var BeenSigned
        , contract = remove BeenSigned
        , morphism = \case
            modl@(PriceModuleModel {pmSignatures = sigs, pmOwner = owner}) -> do
              let sigs' = (delete owner sigs)
              return $ modl {pmSignatures = sigs'}
        }
    , Morphism
        { name = "Sign"
        , match = Not $ Var BeenSigned
        , contract = add BeenSigned
        , morphism = \case
            modl@(PriceModuleModel {pmSignatures = sigs, pmOwner = owner}) -> do
              let sigs' = (owner : sigs)
              return $ modl {pmSignatures = sigs'}
        }
    , Morphism
        { name = "AddPrice"
        , match = Yes
        , contract = (branchIf VectorsSame (remove VectorsSame) (removeAll [VectorHandled, VectorsSame])) >> remove OutputHasHash
        , morphism = \case
            modl@(PriceModuleModel {pmPriceVectorOut = prices}) -> do
              oldPrice <- case prices of
                (x : _) -> return x
                [] -> return $ PricePoint 1653506000 1 1 -- around the time I wrote this line.
              newPrice <- genNewPricePoint oldPrice
              let newPrices = newPrice : (take 47 prices)
              return modl {pmPriceVectorOut = newPrices}
        }
    , Morphism
        { name = "ResetPrices"
        , match = Not $ Var VectorsSame
        , contract = addAll [VectorsSame, VectorHandled] >> remove OutputHasHash
        , morphism = \case
            modl@(PriceModuleModel {pmPriceVectorIn = prices}) ->
              return $ modl {pmPriceVectorOut = prices}
        }
    , Morphism
        { name = "RemoveAllInput"
        , match = Yes
        , contract = removeAll [InputHasAdr, InputHasHash, InputHasNFT, InputHasAll]
        , morphism = \modl -> return $ modl {pmInput = []}
        }
    , Morphism
        { name = "RemoveAllOutput"
        , match = Yes
        , contract = removeAll [OutputHasAdr, OutputHasHash, OutputHasNFT, OutputHasAll]
        , morphism = \modl -> return $ modl {pmOutput = []}
        }
    , Morphism
        { name = "FixInputFromNull"
        , match = Not (Var InputHasAdr :||: Var InputHasHash :||: Var InputHasNFT)
        , contract = addAll [InputHasAdr, InputHasHash, InputHasNFT, InputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmInput = _inp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVIHash modl
                  inputTxOut :: TxOut
                  inputTxOut =
                    TxOut
                      { txOutAddress = adr
                      , txOutValue = assetClassValue nft 1
                      , txOutDatumHash = Just dhsh
                      }
              inputTxIn <- genTxInFromOut inputTxOut
              -- Back to the generation
              return modl {pmInput = [inputTxIn]}
        }
    , Morphism
        { name = "FixInputFromHash"
        , match = (Var InputHasHash) :&&: (Not (Var InputHasAdr :||: Var InputHasNFT))
        , contract = addAll [InputHasAdr, InputHasNFT, InputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmInput = inp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVIHash modl
                  inpTxs = findIndices (\(TxInInfo _ (TxOut _ _ mhsh)) -> mhsh == (Just dhsh)) inp
              -- asdf
              case inpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let inTx@(TxInInfo _ref (TxOut _xadr xval _xdat)) = inp !! n
                      oldVal = assetClassValueOf xval nft
                      newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr newVal (Just dhsh)
                      newTxi = inTx {txInInfoResolved = newTxo}
                      inp' = replaceAt n newTxi inp
                  return $ modl {pmInput = inp'}
                [] -> undefined -- should be unreachable
        }
    , Morphism
        { name = "FixInputFromNFT"
        , match = (Var InputHasNFT) :&&: (Not (Var InputHasAdr :||: Var InputHasHash))
        , contract = addAll [InputHasAdr, InputHasHash, InputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmInput = inp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVIHash modl
                  inpTxs = findIndices (\(TxInInfo _ (TxOut _ val _)) -> assetClassValueOf val nft == 1) inp
              -- asdf
              case inpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let inTx@(TxInInfo _ref (TxOut _xadr xval _xdat)) = inp !! n
                      newTxo = TxOut adr xval (Just dhsh)
                      newTxi = inTx {txInInfoResolved = newTxo}
                      inp' = replaceAt n newTxi inp
                  return $ modl {pmInput = inp'}
                [] -> undefined -- should be unreachable
        }
    , Morphism
        { name = "FixInputFromAddress"
        , match = (Var InputHasAdr) :&&: (Not (Var InputHasHash :||: Var InputHasNFT))
        , contract = addAll [InputHasHash, InputHasNFT, InputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmInput = inp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVIHash modl
                  inpTxs = findIndices (\(TxInInfo _ (TxOut tadr _ _)) -> tadr == adr) inp
              -- asdf
              case inpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let inTx@(TxInInfo _ref (TxOut _xadr xval _xdat)) = inp !! n
                      oldVal = assetClassValueOf xval nft
                      newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr newVal (Just dhsh)
                      newTxi = inTx {txInInfoResolved = newTxo}
                      inp' = replaceAt n newTxi inp
                  return $ modl {pmInput = inp'}
                _ -> undefined -- should be unreachable
        }
    , Morphism
        { name = "FixOutputFromNull"
        , match = Not (Var OutputHasAdr :||: Var OutputHasHash :||: Var OutputHasNFT)
        , contract = addAll [OutputHasAdr, OutputHasHash, OutputHasNFT, OutputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmOutput = _outp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVOHash modl
                  outputTxOut :: TxOut
                  outputTxOut =
                    TxOut
                      { txOutAddress = adr
                      , txOutValue = assetClassValue nft 1
                      , txOutDatumHash = Just dhsh
                      }

              -- Back to the generation
              return modl {pmOutput = [outputTxOut]}
        }
    , Morphism
        { name = "FixOutputFromHash"
        , match = (Var OutputHasHash) :&&: (Not (Var OutputHasAdr :||: Var OutputHasNFT))
        , contract = addAll [OutputHasAdr, OutputHasNFT, OutputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmOutput = outp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVOHash modl
                  outpTxs = findIndices (\(TxOut _ _ mhsh) -> mhsh == (Just dhsh)) outp
              -- asdf
              case outpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let (TxOut _xadr xval _xdat) = outp !! n
                      oldVal = assetClassValueOf xval nft
                      newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr newVal (Just dhsh)
                      outp' = replaceAt n newTxo outp
                  return $ modl {pmOutput = outp'}
                [] -> undefined -- should be unreachable
        }
    , Morphism
        { name = "FixOutputFromNFT"
        , match = (Var OutputHasNFT) :&&: (Not (Var OutputHasAdr :||: Var OutputHasHash))
        , contract = addAll [OutputHasAdr, OutputHasHash, OutputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmOutput = outp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVOHash modl
                  outpTxs = findIndices (\(TxOut _ val _) -> assetClassValueOf val nft == 1) outp
              -- asdf
              case outpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let (TxOut _xadr xval _xdat) = outp !! n
                      newTxo = TxOut adr xval (Just dhsh)
                      outp' = replaceAt n newTxo outp
                  return $ modl {pmOutput = outp'}
                [] -> undefined -- should be unreachable
        }
    , Morphism
        { name = "FixOutputFromAddress"
        , match = (Var OutputHasAdr) :&&: (Not (Var OutputHasHash :||: Var OutputHasNFT))
        , contract = addAll [OutputHasHash, OutputHasNFT, OutputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmOutput = outp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVOHash modl
                  outpTxs = findIndices (\(TxOut tadr _ _) -> tadr == adr) outp
              -- asdf
              case outpTxs of
                (n : _) -> do
                  -- should only be one, but this captures more.
                  -- hmm...
                  let (TxOut _xadr xval _xdat) = outp !! n
                      oldVal = assetClassValueOf xval nft
                      newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr newVal (Just dhsh)
                      outp' = replaceAt n newTxo outp
                  return $ modl {pmOutput = outp'}
                _ -> undefined -- should be unreachable?
        }
    , Morphism
        { name = "FixInputHash"
        , match = (Var InputHasAdr) :&&: (Var InputHasNFT) :&&: (Not (Var InputHasHash))
        , contract = addAll [InputHasHash, InputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmInput = inp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVIHash modl
                  inpTxs = findIndices (\(TxInInfo _ (TxOut tadr val _)) -> tadr == adr && (assetClassValueOf val nft == 1)) inp
              case inpTxs of
                (n : _) -> do
                  let inTx@(TxInInfo _ref (TxOut _xadr xval _xdat)) = inp !! n
                      -- oldVal = assetClassValueOf xval nft
                      -- newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr xval (Just dhsh)
                      newTxi = inTx {txInInfoResolved = newTxo}
                      inp' = replaceAt n newTxi inp
                  return $ modl {pmInput = inp'}
                [] -> do
                  let newVal = assetClassValue nft 1
                      newTxo = TxOut adr newVal (Just dhsh)
                  txi <- genTxInFromOut newTxo
                  return modl {pmInput = [txi]}
        }
    , Morphism
        { name = "FixOutputHash"
        , match = (Var OutputHasAdr) :&&: (Var OutputHasNFT) :&&: (Not (Var OutputHasHash))
        , contract = addAll [OutputHasHash, OutputHasAll]
        , morphism = \case
            modl@(PriceModuleModel {pmOutput = outp, pmAddress = adr, pmValidNFT = nft}) -> do
              let dhsh = pmPVOHash modl
                  inpTxs = findIndices (\(TxOut tadr val _) -> tadr == adr && (assetClassValueOf val nft == 1)) outp
              case inpTxs of
                (n : _) -> do
                  let (TxOut _xadr xval _xdat) = outp !! n
                      -- oldVal = assetClassValueOf xval nft
                      -- newVal = xval <> (assetClassValue nft (1 - oldVal))
                      newTxo = TxOut adr xval (Just dhsh)
                      outp' = replaceAt n newTxo outp
                  return $ modl {pmOutput = outp'}
                [] -> do
                  let newVal = assetClassValue nft 1
                      newTxo = TxOut adr newVal (Just dhsh)
                  return modl {pmOutput = [newTxo]}
        }
    ]

instance HasParameterisedGenerator PriceModuleProp PriceModuleModel where
  parameterisedGenerator = buildGen

instance Enumerable PriceModuleProp where
  enumerated = [minBound .. maxBound]

instance ScriptModel PriceModuleProp PriceModuleModel where
  expect =
    (Var BeenSigned)
      :&&: (Var VectorHandled)
      :&&: (Var InputHasHash)
      :&&: (Var OutputHasHash)

  -- :&&: (Var OutDatumHashed)
  -- :&&: (Var ConfigPresent)
  script mm = applyMintingPolicyScript (mkCtx mm) priceModuleMintingPolicy (Redeemer (toBuiltinData ()))

priceModuleMintingPolicy :: MintingPolicy
priceModuleMintingPolicy = undefined -- TEMP

-- | Make the context from the Model.
mkCtx :: PriceModuleModel -> Context
mkCtx mm = Context $ PlutusTx.toBuiltinData scCtx
  where
    scCtx = ScriptContext txinf (Minting (pmCurrency mm))
    txinf =
      TxInfo
        { txInfoInputs = pmInput mm
        , txInfoOutputs = pmOutput mm
        , txInfoFee = mempty
        , txInfoMint = pmMinted mm
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = always
        , txInfoSignatories = pmSignatures mm
        , txInfoData = [(pmPVIHash mm, makeDatum $ pmPriceVectorIn mm), (pmPVOHash mm, makeDatum $ pmPriceVectorOut mm)]
        , txInfoId = "" -- Temp?
        }

-- | Safe version of `!!`.
indexVal :: [a] -> Int -> Maybe a
indexVal lst n = fst <$> uncons (drop n lst)

checkTxOut :: CurrencySymbol -> Integer -> Address -> DatumHash -> TxOut -> Bool
checkTxOut cs n adr dhsh txo =
  (txOutAddress txo == adr)
    && (txOutDatumHash txo == Just dhsh)
    && (n == valueOf (txOutValue txo) cs "")

checkTxOutAC :: AssetClass -> Integer -> Address -> DatumHash -> TxOut -> Bool
checkTxOutAC ac n adr dhsh txo =
  (txOutAddress txo == adr)
    && (txOutDatumHash txo == Just dhsh)
    && (n == assetClassValueOf (txOutValue txo) ac)

checkTxOutHash :: DatumHash -> TxOut -> Bool
checkTxOutHash dhsh txo =
  txOutDatumHash txo == Just dhsh

checkTxOutVal :: AssetClass -> Integer -> TxOut -> Bool
checkTxOutVal ac n txo =
  n == assetClassValueOf (txOutValue txo) ac

checkTxOutAdr :: Address -> TxOut -> Bool
checkTxOutAdr adr txo =
  (txOutAddress txo == adr)

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

{- | Insert an element at position n of a list.
 Note that if n > (length lst), the element
 is inserted at the end.
-}
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = (take n xs) ++ [x] ++ (drop n xs)

-- insertAt n x xs = let (!ys, !zs) = splitAt n xs in ys ++ [x] ++ zs
-- Tried various versions with Criterion;
-- went with most efficient version.

{- | Replace an element at position n of a list.
 Note that if n > (length lst), the element
 is inserted at the end, and nothing is replaced.
-}
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs = (take n xs) ++ [x] ++ (drop (n + 1) xs)

-- replaceAt n x xs = let (!ys, !zs) = splitAt n xs in ys ++ [x] ++ (drop 1 zs)

genTxInFromOut :: TxOut -> Gen TxInInfo
genTxInFromOut txo = do
  -- More definition of Txs
  txRefId <- Gen.txId
  txRefIx <- fromIntegral <$> int (linear 0 3)

  let inputTxRef :: TxOutRef
      inputTxRef =
        TxOutRef
          { txOutRefId = txRefId
          , txOutRefIdx = txRefIx
          }

      inputTxIn :: TxInInfo
      inputTxIn =
        TxInInfo
          { txInInfoOutRef = inputTxRef
          , txInInfoResolved = txo
          }
  return inputTxIn

{- | Create a new price point by slightly
 modifying an existing price point.
-}
genNewPricePoint :: PricePoint -> Gen PricePoint
genNewPricePoint PricePoint {ppTime = tim, ppAdaPerUsd = apu, ppUsdPerAda = _upa} = do
  -- Convert to Double and back; otherwise the size
  -- of the rational could grow out of hand.
  mdf <- fromRational @Double <$> Gen.rationalRange 1_000_000 0.8 1.2
  let orig = fromRational apu
      tempPrice' = orig * mdf
      tempPrice = approxRational tempPrice' 0.000_01 -- maybe change epsilon?
  newPrice <-
    if (tempPrice /= 0)
      then return tempPrice
      else do
        mdf' <- fromRational @Double <$> Gen.rationalRange 1_000_000 1 1.2
        let tempPrice2 = orig * mdf'
        return $ approxRational tempPrice2 0.000_01
  dfTime <- fromIntegral <$> int (linear 300 18_000)
  let newTime = tim + dfTime
  return $ PricePoint {ppTime = newTime, ppAdaPerUsd = newPrice, ppUsdPerAda = recip newPrice}

-- Maybe generate a different modifier for USD/ADA?

{- | Monadic equivalent to @'take' n . 'iterate'@.
 (Taken from monad-extras and slightly modified)
-}
iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM n _ _ | n <= 0 = return []
iterateM n f x = do
  x' <- f x
  (x' :) `liftM` iterateM (n - 1) f x'
