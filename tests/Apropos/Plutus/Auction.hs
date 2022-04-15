module Apropos.Plutus.Auction (
  BidProp (..),
  BidModel (..),
  AuctionProp (..),
  AuctionModel (..),
  spec,
) where

import Apropos
import Apropos.Plutus.SingletonValue (SingletonValue, SingletonValueProp (..))
import Apropos.Plutus.Integer (IntegerProp(..))
import Apropos.Script ( ScriptModel(..) )

import Auction(auctionContract)

import Control.Lens (lens)
import Control.Monad ( liftM2 )
import Gen (pubKeyHash)
import Plutus.V1.Ledger.Api (PubKeyHash,Datum (Datum))

import Plutarch(compile,(#))
import Plutarch.Lift(pconstant)

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

data AuctionStateProp
  = Selling SingletonValueProp
  | CurrentBid BidProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data AuctionStateModel = AuctionStateModel
  { selling :: SingletonValue
  , currentBid :: BidModel
  }
  deriving stock (Eq, Show)

newtype BidProp
  = BidAmt SingletonValueProp
  -- TODO props for bidder?
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data BidModel = BidModel
  { bidder :: PubKeyHash
  , bid :: SingletonValue
  }
  deriving stock (Eq, Show)


data AuctionProp
  = State AuctionStateProp
  | NewBid BidProp
  | NewBidIsLarger
  deriving stock (Eq,Ord,Show,Generic)
  deriving anyclass (Enumerable,Hashable)


data AuctionModel
  = AuctionModel
    {state :: AuctionStateModel
    ,newBid :: BidModel
    }
    deriving stock (Eq,Show)

-- Bid

instance HasAbstractions BidProp BidModel where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "bid"
          , propertyAbstraction = abstractsProperties BidAmt
          , productModelAbstraction = lens bid (\am bid' -> am {bid = bid'})
          }
    ]

instance LogicalModel BidProp where
  logic = abstractionLogic @BidModel

instance HasLogicalModel BidProp BidModel where
  satisfiesProperty (BidAmt p) = satisfiesProperty p . bid

instance HasPermutationGenerator BidProp BidModel where
  generators = abstractionMorphisms

baseBidGen :: Gen BidModel
baseBidGen = BidModel <$> pubKeyHash <*> genSatisfying @SingletonValueProp Yes

instance HasParameterisedGenerator BidProp BidModel where
  parameterisedGenerator = buildGen baseBidGen

-- AuctionState

instance HasAbstractions AuctionStateProp AuctionStateModel where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "bid"
          , propertyAbstraction = abstractsProperties CurrentBid
          , productModelAbstraction = lens currentBid (\am bid' -> am {currentBid = bid'})
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "selling"
          , propertyAbstraction = abstractsProperties Selling
          , productModelAbstraction = lens selling (\am selling' -> am {selling = selling'})
          }
    ]

instance LogicalModel AuctionStateProp where
  logic = abstractionLogic @AuctionStateModel
    :&&: Var (CurrentBid (BidAmt (Amt IsPositive)))
    :&&: Var (Selling (Amt IsPositive))

instance HasLogicalModel AuctionStateProp AuctionStateModel where
  satisfiesProperty (Selling p) = satisfiesProperty p . selling
  satisfiesProperty (CurrentBid p) = satisfiesProperty p . currentBid

instance HasPermutationGenerator AuctionStateProp AuctionStateModel where
  generators = abstractionMorphisms

instance HasParameterisedGenerator AuctionStateProp AuctionStateModel where
  parameterisedGenerator = buildGen baseAuctionStateGen

baseAuctionStateGen :: Gen AuctionStateModel
baseAuctionStateGen =
  AuctionStateModel
    <$> genSatisfying (Var $ Amt IsPositive)
    <*> genSatisfying (Var $ BidAmt $ Amt IsPositive)

-- Auctionstate

instance HasAbstractions AuctionProp AuctionModel where
  abstractions =
    [ WrapAbs $
      ProductAbstraction
        { abstractionName = "state"
        , propertyAbstraction = abstractsProperties State
        , productModelAbstraction = lens state (\a s -> a{state=s})
        }
    , WrapAbs $
      ProductAbstraction
        { abstractionName = "new bid"
        , propertyAbstraction = abstractsProperties NewBid
        , productModelAbstraction = lens newBid (\a s -> a{newBid=s})
        }
    ]

instance LogicalModel AuctionProp where
  logic = abstractionLogic @AuctionModel

instance HasLogicalModel AuctionProp AuctionModel where
  satisfiesProperty (NewBid p) = satisfiesProperty p . newBid
  satisfiesProperty (State p) = satisfiesProperty p . state
  satisfiesProperty NewBidIsLarger = liftM2 (>) (bid . newBid) (bid . currentBid . state)

instance HasPermutationGenerator AuctionProp AuctionModel where
  generators = abstractionMorphisms

instance HasParameterisedGenerator AuctionProp AuctionModel where
  parameterisedGenerator = buildGen baseAuctionGen

--- TODO these FromModel functions should be ScriptModel methods
-- and the boiler plate should be a defaul script implementation


datumFromModel :: AuctionModel -> Datum
datumFromModel = undefined

redemerFromModel :: AuctionModel -> Datum
redemerFromModel = undefined

scriptContextFromModel :: AuctionModel -> Datum
scriptContextFromModel = undefined

instance ScriptModel AuctionProp AuctionModel where
  expect _ = Var NewBidIsLarger
  script _ m = let
    datum :: Datum
    datum = datumFromModel m
    redemer :: Datum
    redemer = redemerFromModel m
    sc :: Datum
    sc = scriptContextFromModel m
      in compile $ auctionContract # pconstant datum # pconstant redemer # pconstant sc

baseAuctionGen :: Gen AuctionModel
baseAuctionGen =
  AuctionModel
    <$> genSatisfying @AuctionStateProp Yes
    <*> genSatisfying @BidProp Yes

spec :: Spec
spec = do
  it "auction model samples" $ sampleGenTest (Apropos :: AuctionModel :+ AuctionProp)
  fromHedgehogGroup $ runGeneratorTestsWhere
    (Apropos :: BidModel :+ BidProp) "bid generator" Yes
  fromHedgehogGroup $ runGeneratorTestsWhere
    (Apropos :: AuctionStateModel :+ AuctionStateProp) "auction state generator" Yes


