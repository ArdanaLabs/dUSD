module Apropos.Plutus.Management (
  -- spec,
) where

import Apropos
import Data.List (uncons, length, drop)
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
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue, flattenValue, Value, assetClass)
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
  deriving stock (Show, Eq, Ord, Enum, Bounded)


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
            valHash <- Gen.validatorHash
            -- TODO : Make this align with the
            -- address below.
            cs  <- Gen.hexString @CurrencySymbol
            -- TODO : Replace this with one that only
            -- generates validator addresses.
            adr <- Gen.address
            owner <- Gen.pubKeyHash
            numSigs <- linear 0 5
            sigs' <- replicateM numSigs Gen.pubKeyHash
            posSigs <- linear 0 numSigs
            let sigs = (take posSigs) ++ [owner] ++ (drop posSigs)
            numCurrencies <- linear 2 12
            inCurrencies' <- replicateM numCurrencies (Gen.hexString @CurrencySymbol) 
            let inCurrencies = owner : inCurrencies'
                inDatHash    = datumHash $ Datum $ PlutusTx.toData $ inCurrencies
            inNft <- Gen.hexString @CurrencySymbol
            -- okay
            return $ ManagementModel
              { mmCurrencies  = inCurrencies
              , mmSignatures  = sigs
              , mmInDatumHash = inDatHash
              , mmOwnCurrency = cs
              , mmCurChoice = 0
              , mmMinted    = assetClassValue (assetClass cs "") 1
              , mmOwner     = owner
              , mmInput     = [] -- TODO
              , mmOutput    = [] -- TODO
              , mmAddress   = adr
              , mmOutDatum  = inCurrencies -- TEMP
              , mmOutDatumHash = inDatHash
              , mmInNFT = inNft
              }
        }
    
    ]
  generators = [] -- TODO


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
    | dhsh <- datumHash $ Datum $ PlutusTx.toData $ mmCurrencies modl
    = dhsh == mmInDatumHash modl
  satisfiesProperty OutDatumHashed modl
    | dhsh <- datumHash $ Datum $ PlutusTx.toData $ mmOutDatum modl
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
indexVal lst n = fst <$> uncons $ drop n lst



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

