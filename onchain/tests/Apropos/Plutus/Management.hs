module Apropos.Plutus.Auction (
  -- spec,
) where

import Apropos
import Data.List (uncons, length, drop)
import GHC.Generics (Generic)
import Gen
import Test.Syd
import Test.Syd.Hedgehog

import Plutarch (compile, (#))
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude qualified as PPrelude
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue, flattenValue, Value)

-- | The model for the properties.
type ManagementModel = ManagementModel
  { mmCurrencies :: [CurrencySymbol]  -- The currencies in the datum.
  , mmSignatures :: [PubKeyHash]      -- Signatures present in the Tx.
  , mmCurChoice :: Int
  , mmMinted :: Value
  , mmOwner  :: PubKeyHash -- The 'Owner' of this policy; baked into the policy.
  , mmInput  :: [TxInInfo] -- The inputs to the minting policy
  , mmOutput :: [TxOut]    -- The outputs of this policy.
  -- , mmAddress :: MintingPolicyHash -- The address of this minting policy.
  , mmAddress :: CurrencySymbol -- The address of this minting policy.
  , mmOutDatum :: [CurrencySymbol] -- The datum to the script.
  } deriving stock (Show, Eq, Generic)

data ManagementProp
  = BeenSigned
  | MintsOne         -- only one item is minted.
  | ValidCurChoice   -- i.e. 0 <= mmCurChoice < length mmCurrencies
  | MintsChoice      -- i.e. that it actually mints the choice.
  | MintsCorrectly   -- i.e. that the value minted matches the choice and == 1.
  | ConfigPresent
  | ConfigReturned
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Enumerable ManagementProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel ManagementProp where
  logic = 
    (Var MintsCorrectly :->: Var MintsOne) 
      :&&: (Var MintsCorrectly :->: Var MintsChoice) 
      :&&: (Var MintsChoice    :->: Var ValidCurChoice)

instance HasLogicalModel ManagementProp ManagementModel where
  satisfiesProperty BeenSigned mod = (mmOwner mod) `elem` (mmSignatures mod)
  satisfiesProperty MintsOne   mod = let val = flattenValue (mmMinted mod) in
    case uncons val of
      (Just ((_,_,n),[])) -> n == 1
      _ -> False
  satisfiesProperty ValidCurChoice mod = let n = mmCurChoice mod in
    (0 <= n) && (n < length (mmCurrencies mod))
  satisfiesProperty MintsChoice mod
    | (mmCurChoice mod) < 0 = False -- This guard has to be first.
    | (Just cur) <- indexVal (mmCurrencies mod) (mmCurChoice mod)
    = 1 == valueOf (mmMinted mod) cur "" 
    | otherwise = False
  satisfiesProperty MintsCorrectly mod = 
    (satisfiesProperty MintsOne mod) && (satisfiesProperty MintsChoice mod)
  

-- | Safe version of `!!`.
indexVal :: [a] -> Int -> Maybe a
indexVal lst n = fst <$> uncons $ drop n lst





