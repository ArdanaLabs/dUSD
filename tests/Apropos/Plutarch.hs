{-# LANGUAGE RecordWildCards #-}

module Apropos.Plutarch () where

import Apropos
import Apropos.Script.Model
import Data.Maybe (isJust)
import GHC.Generics
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Crypto (PubKey)
import Plutus.V1.Ledger.Value (AssetClass)
--import Test.Tasty (TestTree, testGroup)
--import Test.Tasty.Hedgehog (fromGroup)

{-
    Port of https://github.com/ArdanaLabs/ardana-dollar/blob/main/test/Test/ArdanaDollar/PriceOracle/OnChain/Model/Proper.hs
    TODO:
    1. UniqueMap and AssocMap from PlutusTx are used by the original code. Need to figure that out, currently
    stubs are used.
    2. Scripts don't exist yet, so the script runner is undefined.
    3. At the very bottom, oracleCurrencySymbol and oracleMintingParams: need to figure out what the Plutarch
    equivalent logic is, but it also uses actual written scripts.
-}

-- STUBS

pubKey :: a
pubKey = undefined

pubKeyHash :: a
pubKeyHash = undefined

knownWallet :: a
knownWallet = undefined

scriptCurrencySymbol :: a
scriptCurrencySymbol = undefined

oracleMintingPolicy :: a
oracleMintingPolicy = undefined

newtype UniqueMap a b = UM () deriving stock (Show)

assocMapLookup :: a -> b -> c
assocMapLookup = undefined

uniqueMapNull :: UniqueMap a b -> Bool
uniqueMapNull _ = True

-- ===

data OracleMintingParams = OracleMintingParams
    { oracleMintingParamsOperator :: PubKey
    , oracleMintingParamsOperatorPkh :: PubKeyHash
    }
    deriving stock (Eq, Show, Generic)

data TransactorParams = NoSigner | JustSignedBy Integer | SignedByWithValue Integer Value
    deriving stock (Show)

data StateUTXOParams = StateUTXOParams
    { stateTokenValue :: Value
    , stateDatumValue :: Maybe TestDatumParameters
    }
    deriving stock (Show)

data TestDatumParameters = TestDatumParameters
    { signedByWallet :: Integer
    , timeStamp :: Integer
    , fiatPriceFeedData :: UniqueMap BuiltinByteString Integer
    , cryptoPriceFeedData :: UniqueMap AssetClass Integer
    }
    deriving stock (Show)

data PriceOracleModel
    = PriceOracleStateMachineModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , timeRangeLowerBound :: Integer
        , timeRangeUpperBound :: Integer
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , inputParams :: StateUTXOParams
        , outputParams :: StateUTXOParams
        , peggedCurrency :: BuiltinByteString
        , valueRetrieved :: Maybe ([Value], Address)
        }
    | PriceOracleMinterModel
        { stateNFTCurrency :: (CurrencySymbol, TokenName)
        , timeRangeLowerBound :: Integer
        , timeRangeUpperBound :: Integer
        , ownerWallet :: Integer
        , transactorParams :: TransactorParams
        , outputParams :: StateUTXOParams
        }
    deriving stock (Show)

data PriceOracleProp
    = PriceOracleMintingPolicyContext
    | PriceOracleStateMachineContext
    | OutputDatumTimestampIsInRange
    | RangeWithinSizeLimit
    | OutputDatumSignedByOwner
    | TransactionSignedByOwner
    | StateTokenReturned
    | InputDatumIsCorrectType
    | OutputDatumIsCorrectType
    | OwnerIsRetrievingValue
    | InputPriceTrackingDatumIsEmpty
    | OutputPriceTrackingDatumIsEmpty
    deriving stock (Enum, Eq, Ord, Bounded, Show)

instance Enumerable PriceOracleProp where
    enumerated = [minBound .. maxBound]

instance LogicalModel PriceOracleProp where
    logic =
        All
            [ ExactlyOne (Var <$> [PriceOracleMintingPolicyContext, PriceOracleStateMachineContext])
            , Some
                ( Var
                    <$> [ InputDatumIsCorrectType
                        , OwnerIsRetrievingValue
                        ]
                )
                :->: Var PriceOracleStateMachineContext
            , -- parsing will fail before we can check properties of the datum hence these implications
              Var OutputDatumSignedByOwner :->: Var OutputDatumIsCorrectType
            , Var OutputDatumTimestampIsInRange :->: Var OutputDatumIsCorrectType
            , Var OutputPriceTrackingDatumIsEmpty :->: Var OutputDatumIsCorrectType
            , Var InputPriceTrackingDatumIsEmpty :->: Var InputDatumIsCorrectType
            ]

instance HasLogicalModel PriceOracleProp PriceOracleModel where
    satisfiesProperty OutputDatumTimestampIsInRange = outputDatumTimestampIsInRange
    satisfiesProperty RangeWithinSizeLimit = rangeWithinSizeLimit
    satisfiesProperty OutputDatumSignedByOwner = outputDatumSignedByOwner
    satisfiesProperty TransactionSignedByOwner = transactionSignedByOwner
    satisfiesProperty StateTokenReturned = stateTokenReturned
    satisfiesProperty InputDatumIsCorrectType = inputDatumIsCorrectType
    satisfiesProperty OutputDatumIsCorrectType = outputDatumIsCorrectType
    satisfiesProperty PriceOracleMintingPolicyContext = isMinterModel
    satisfiesProperty PriceOracleStateMachineContext = isScriptModel
    satisfiesProperty OwnerIsRetrievingValue = ownerIsRetrievingValue
    satisfiesProperty OutputPriceTrackingDatumIsEmpty = outputPriceTrackingDatumIsEmpty
    satisfiesProperty InputPriceTrackingDatumIsEmpty = inputPriceTrackingDatumIsEmpty

isMinterModel :: PriceOracleModel -> Bool
isMinterModel PriceOracleMinterModel{} = True
isMinterModel _ = False

isScriptModel :: PriceOracleModel -> Bool
isScriptModel PriceOracleStateMachineModel{} = True
isScriptModel _ = False

outputDatumTimestampIsInRange :: PriceOracleModel -> Bool
outputDatumTimestampIsInRange model =
    case stateDatumValue $ outputParams model of
        Nothing -> False
        Just so -> timeRangeLowerBound model <= timeStamp so && timeStamp so <= timeRangeUpperBound model

rangeWithinSizeLimit :: PriceOracleModel -> Bool
rangeWithinSizeLimit model =
    let rangeLen = timeRangeUpperBound model - timeRangeLowerBound model
     in 0 < rangeLen && rangeLen <= 10000

outputDatumSignedByOwner :: PriceOracleModel -> Bool
outputDatumSignedByOwner model =
    case stateDatumValue $ outputParams model of
        Nothing -> False
        Just so -> signedByWallet so == ownerWallet model

transactionSignedByOwner :: PriceOracleModel -> Bool
transactionSignedByOwner model =
    case transactorParams model of
        NoSigner -> False
        JustSignedBy signer -> signer == ownerWallet model
        SignedByWithValue signer _ -> signer == ownerWallet model

stateTokenReturned :: PriceOracleModel -> Bool
stateTokenReturned model =
    let c = fst $ correctNFTCurrency $ oracleMintingParams $ ownerWallet model
     in case assocMapLookup c $ getValue $ stateTokenValue $ outputParams model of
            Nothing -> False
            Just so -> case assocMapLookup (snd $ stateNFTCurrency model) so of
                Just (1 :: Integer) -> True
                _ -> False

inputDatumIsCorrectType :: PriceOracleModel -> Bool
inputDatumIsCorrectType PriceOracleStateMachineModel{..} = isJust $ stateDatumValue inputParams
inputDatumIsCorrectType _ = False

outputDatumIsCorrectType :: PriceOracleModel -> Bool
outputDatumIsCorrectType model = isJust $ stateDatumValue $ outputParams model

ownerIsRetrievingValue :: PriceOracleModel -> Bool
ownerIsRetrievingValue PriceOracleStateMachineModel{..} = isJust valueRetrieved
ownerIsRetrievingValue _ = False

outputPriceTrackingDatumIsEmpty :: PriceOracleModel -> Bool
outputPriceTrackingDatumIsEmpty model =
    case stateDatumValue $ outputParams model of
        Nothing -> False
        Just so -> uniqueMapNull (fiatPriceFeedData so) && uniqueMapNull (cryptoPriceFeedData so)

inputPriceTrackingDatumIsEmpty :: PriceOracleModel -> Bool
inputPriceTrackingDatumIsEmpty model@PriceOracleStateMachineModel{} =
    case stateDatumValue $ inputParams model of
        Nothing -> False
        Just so -> uniqueMapNull (fiatPriceFeedData so) && uniqueMapNull (cryptoPriceFeedData so)
inputPriceTrackingDatumIsEmpty _ = False

correctNFTCurrency :: OracleMintingParams -> (CurrencySymbol, TokenName)
correctNFTCurrency params = (oracleCurrencySymbol params, "PriceTracking")

instance HasParameterisedGenerator PriceOracleProp PriceOracleModel where
  parameterisedGenerator = undefined

instance ScriptModel PriceOracleProp PriceOracleModel where
    expect _ =
        All
            [ Var PriceOracleStateMachineContext
                :->: All
                    ( Var
                        <$> [ OutputDatumTimestampIsInRange
                            , RangeWithinSizeLimit
                            , OutputDatumSignedByOwner
                            , TransactionSignedByOwner
                            , StateTokenReturned
                            , InputDatumIsCorrectType
                            , OutputDatumIsCorrectType
                            ]
                    )
            , Var PriceOracleMintingPolicyContext
                :->: All
                    ( Var
                        <$> [ OutputDatumTimestampIsInRange
                            , OutputPriceTrackingDatumIsEmpty
                            , RangeWithinSizeLimit
                            , TransactionSignedByOwner
                            , StateTokenReturned
                            , OutputDatumIsCorrectType
                            , OutputDatumSignedByOwner
                            ]
                    )
            ]
    script _ = undefined

  {-
testTree :: TestTree
testTree =
    testGroup
        "cos"
        [ fromGroup $ runGeneratorTestsWhere Apropos "" Yes
        , fromGroup $ runScriptTestsWhere Apropos "" Yes
        ]
        -}

oracleCurrencySymbol ::
    OracleMintingParams ->
    CurrencySymbol
oracleCurrencySymbol params =
    scriptCurrencySymbol (oracleMintingPolicy params)

oracleMintingParams :: Integer -> OracleMintingParams
oracleMintingParams walletIdx = OracleMintingParams ownerPubKey ownerPubKeyHash
  where
    ownerPubKey :: PubKey
    ownerPubKey = pubKey $ knownWallet walletIdx
    ownerPubKeyHash :: PubKeyHash
    ownerPubKeyHash = pubKeyHash ownerPubKey


