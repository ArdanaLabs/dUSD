{-# OPTIONS_GHC -Wno-unused-imports #-}

module Apropos.Plutus.Management (
  -- spec,
) where

import Apropos
import Control.Monad (replicateM)
import Data.List (uncons, length, drop, delete)
import GHC.Generics (Generic)
import Gen qualified
import Test.Syd
import Test.Syd.Hedgehog

import Codec.Serialise (serialise)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (Blake2b_224 (Blake2b_224), HashAlgorithm)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import PlutusTx.Builtins qualified as Builtins

-- import Ledger.Scripts (datumHash) -- will this work?

import Plutarch (compile, (#))
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude qualified as PPrelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue, flattenValue, Value, assetClass, valueOf)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified

-- | The model for the properties.
data ManagementModel = ManagementModel
  { mmCurrencies :: [CurrencySymbol]  -- The currencies in the datum.
  , mmSignatures :: [PubKeyHash]      -- Signatures present in the Tx.
  , mmInDatumHash :: DatumHash
  , mmOwnCurrency :: CurrencySymbol
  , mmCurChoice :: Int
  , mmMinted :: Value
  , mmOwner  :: PubKeyHash -- The 'Owner' of this policy; baked into the policy.
  , mmInput  :: [TxInInfo] -- The inputs to the minting policy
  , mmOutput :: [TxOut]    -- The outputs of this policy.
  -- , mmAddress :: MintingPolicyHash -- The address of this minting policy.
  -- , mmAddress :: CurrencySymbol -- The address of this minting policy.
  , mmAddress :: Address
  , mmOutDatum :: [CurrencySymbol] -- The datum to the script.
  , mmOutDatumHash :: DatumHash
  , mmInNFT :: CurrencySymbol -- the input NFT.
  } deriving stock (Show, Eq, Generic)

data ManagementProp
  = BeenSigned
  | InDatumHashed    -- mmInDatumHash modl  == datumHash (mmCurrencies modl)
  | OutDatumHashed   -- mmOutDatumHash modl == datumHash (mmOutDatum   modl)
  | MintsOne         -- only one item is minted.
  | ValidCurChoice   -- i.e. 0 <= mmCurChoice < length mmCurrencies
  | MintsChoice      -- i.e. that it actually mints the choice.
  | MintsCorrectly   -- i.e. that the value minted matches the choice and == 1.
  | ConfigPresent    -- Config is present in the input
  | ConfigReturned
  | OwnAtInBase  -- ownCurrencySymbol is the first one in the datum.
  | OwnAtOutBase 
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)


instance HasPermutationGenerator ManagementProp ManagementModel where
  sources =
    [ Source
        { sourceName = "Correctly Formed"
        , covers = All 
          [ Var BeenSigned
          , Var InDatumHashed
          , Var OutDatumHashed
          , Var ConfigPresent
          , Var ConfigReturned
          , Var OwnAtInBase
          , Var OwnAtOutBase
          ]
        , gen = do
            -- valHash <- Gen.validatorHash
            -- TODO : Make this align with the
            -- address below.
            cs  <- Gen.hexString @CurrencySymbol
            -- TODO : Replace this with one that only
            -- generates validator addresses.
            adr <- Gen.address
            owner <- Gen.pubKeyHash
            numSigs <- int (linear 0 5)
            sigs' <- replicateM numSigs Gen.pubKeyHash
            posSigs <- int (linear 0 numSigs)
            let sigs = (take posSigs sigs') ++ [owner] ++ (drop posSigs sigs')
            numCurrencies <- int (linear 2 12)
            inCurrencies' <- replicateM numCurrencies (Gen.hexString @CurrencySymbol) 
            let inCurrencies = cs : inCurrencies'
                inDatHash    = datumHash $ Datum $ PlutusTx.toBuiltinData $ inCurrencies
            inNft <- Gen.hexString @CurrencySymbol

            -- Generating the Txs
            let inputTxOut :: TxOut
                inputTxOut = TxOut
                  { txOutAddress = adr
                  , txOutValue   = Value.singleton inNft "" 1 -- might need to add 2 ADA.
                  , txOutDatumHash = Just inDatHash
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

                -- The value minted
                outputTxOut :: TxOut
                outputTxOut = TxOut
                  { txOutAddress = adr -- Or should it be something else?
                  , txOutValue = Value.singleton cs "" 1
                  , txOutDatumHash = Nothing -- TEMP
                  }
           
            -- Constructing the model
            return $ ManagementModel
              { mmCurrencies  = inCurrencies
              , mmSignatures  = sigs
              , mmInDatumHash = inDatHash
              , mmOwnCurrency = cs
              , mmCurChoice = 0
              , mmMinted    = assetClassValue (assetClass cs "") 1
              , mmOwner     = owner
              , mmInput     = [inputTxIn] -- Temp?
              , mmOutput    = [inputTxOut, outputTxOut] -- Temp?
              , mmAddress   = adr
              , mmOutDatum  = inCurrencies -- Temp?
              , mmOutDatumHash = inDatHash
              , mmInNFT = inNft
              }
        }
    
    ] -- should probably add more.
  generators = 
    [ Morphism
        { name = "Unsign"
        , match = Var BeenSigned
        , contract = remove BeenSigned
        , morphism = \case
          modl@(ManagementModel {mmSignatures = sigs, mmOwner = owner}) -> do
            let sigs' = (delete owner sigs)
            return $ modl {mmSignatures = sigs'}
        }
    , Morphism
        { name = "Sign"
        , match = Not $ Var BeenSigned
        , contract = addAll [BeenSigned]
        , morphism = \case
          modl@(ManagementModel {mmSignatures = sigs, mmOwner = owner}) -> do
            let sigs' = (owner:sigs)
            return $ modl {mmSignatures = sigs'} 
        }
    , Morphism
        { name = "UnhashIn"
        , match = Var InDatumHashed
        , contract = remove InDatumHashed
        , morphism = \case
          modl@(ManagementModel {}) -> do
            inHsh' <- Gen.datumHash
            return $ modl {mmInDatumHash = inHsh'}
        }
    , Morphism
        { name = "HashIn"
        , match = Not $ Var InDatumHashed
        , contract = addAll [InDatumHashed]
        , morphism = \case
          modl@(ManagementModel {mmCurrencies = inDatum}) -> do
            let inHsh' = datumHash $ makeDatum inDatum
            return $ modl {mmInDatumHash = inHsh'}
        }
    , Morphism
        { name = "UnhashOut"
        , match = Var OutDatumHashed
        , contract = remove OutDatumHashed
        , morphism = \case
          modl@(ManagementModel {}) -> do
            outHsh' <- Gen.datumHash
            return $ modl {mmOutDatumHash = outHsh'}
        }
    , Morphism
        { name = "HashOut"
        , match = Not $ Var OutDatumHashed
        , contract = addAll [OutDatumHashed]
        , morphism = \case
          modl@(ManagementModel {mmOutDatum = outDatum}) -> do
            let outHsh' = datumHash $ makeDatum outDatum
            return $ modl {mmOutDatumHash = outHsh'}
        }
    ]


instance Enumerable ManagementProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel ManagementProp where
  logic = 
    (Var MintsCorrectly :->: Var MintsOne) 
      :&&: (Var MintsCorrectly :->: Var MintsChoice) 
      :&&: (Var MintsChoice    :->: Var ValidCurChoice)

instance HasLogicalModel ManagementProp ManagementModel where
  satisfiesProperty BeenSigned modl = (mmOwner modl) `elem` (mmSignatures modl)
  satisfiesProperty  InDatumHashed modl
    | dhsh <- datumHash $ Datum $ PlutusTx.toBuiltinData $ mmCurrencies modl
    = dhsh == mmInDatumHash modl
  satisfiesProperty OutDatumHashed modl
    | dhsh <- datumHash $ Datum $ PlutusTx.toBuiltinData $ mmOutDatum modl
    = dhsh == mmOutDatumHash modl  
  -- These next few apparently aren't needed?
  satisfiesProperty MintsOne   modl = let val = flattenValue (mmMinted modl) in
    case uncons val of
      (Just ((_,_,n),[])) -> n == 1
      _ -> False
  satisfiesProperty ValidCurChoice modl = let n = mmCurChoice modl in
    (0 <= n) && (n < length (mmCurrencies modl))
  satisfiesProperty MintsChoice modl
    | (mmCurChoice modl) < 0 = False -- This guard has to be first.
    | (Just cur) <- indexVal (mmCurrencies modl) (mmCurChoice modl)
    = 1 == valueOf (mmMinted modl) cur "" 
    | otherwise = False
  satisfiesProperty MintsCorrectly modl = 
    (satisfiesProperty MintsOne modl) && (satisfiesProperty MintsChoice modl)
  -- Back to the necessary ones.
  -- Note that this doesn't check that the
  -- datum hash is indeed the hash of the 
  -- currency list; it just checks that
  -- the hahs is indeed in the input utxo.
  satisfiesProperty ConfigPresent  modl
    | inps <- map txInInfoResolved (mmInput modl)
    = any (checkTxOut (mmOwnCurrency modl) 1 (mmAddress modl) (mmInDatumHash  modl)) inps
  satisfiesProperty ConfigReturned modl
    = any (checkTxOut (mmOwnCurrency modl) 1 (mmAddress modl) (mmOutDatumHash modl)) (mmOutput modl)
  satisfiesProperty OwnAtInBase  modl
    | (Just (cur0,_)) <- uncons (mmCurrencies modl)
    = cur0 == mmOwnCurrency modl
    | otherwise = False
  satisfiesProperty OwnAtOutBase modl
    | (Just (cur0,_)) <- uncons (mmOutDatum modl)
    = cur0 == mmOwnCurrency modl
    | otherwise = False


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

