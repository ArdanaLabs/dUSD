module Apropos.Plutus.Auction
  (
  BidProp(..),
  BidModel(..),
  AuctionProp(..),
  AuctionModel(..),
  spec,
  ) where

import Apropos
import Apropos.Plutus.SingletonValue(SingletonValue,SingletonValueProp(..))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api ( PubKeyHash )
import Gen(pubKeyHash)
import Control.Lens ( lens )

import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

data AuctionProp
    = Selling SingletonValueProp
    | CurrentBid BidProp
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Enumerable)

data AuctionModel = AuctionModel
    { selling :: SingletonValue
    , currentBid :: BidModel
    }
    deriving stock (Eq, Show)

newtype BidProp
  = BidAmt SingletonValueProp
  -- TODO props for bidder?
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Enumerable)


data BidModel = BidModel
  { bidder :: PubKeyHash
  , bid :: SingletonValue
  }
  deriving stock (Eq, Show)

-- Bid

instance HasAbstractions BidProp BidModel where
  abstractions =
    [WrapAbs $
      ProductAbstraction
        { abstractionName = "bid"
        , propertyAbstraction = abstractsProperties BidAmt
        , productModelAbstraction = lens bid (\am bid' -> am{bid=bid'})
        }
    ]

instance LogicalModel BidProp where
    logic = abstractionLogic @BidModel

instance HasLogicalModel BidProp BidModel where
  satisfiesProperty (BidAmt p) bidModel = satisfiesProperty p (bid bidModel)

instance HasPermutationGenerator BidProp BidModel where
  generators = abstractionMorphisms

baseBidGen :: Gen BidModel
baseBidGen = BidModel <$> pubKeyHash <*> genSatisfying @SingletonValueProp Yes

instance HasParameterisedGenerator BidProp BidModel where
  parameterisedGenerator = buildGen baseBidGen

-- Auction

instance HasAbstractions AuctionProp AuctionModel where
  abstractions =
    [WrapAbs $
      ProductAbstraction
        { abstractionName = "bid"
        , propertyAbstraction = abstractsProperties CurrentBid
        , productModelAbstraction = lens currentBid (\am bid' -> am{currentBid=bid'})
        }
    ,WrapAbs $
      ProductAbstraction
        { abstractionName = "selling"
        , propertyAbstraction = abstractsProperties Selling
        , productModelAbstraction = lens selling (\am selling' -> am{selling=selling'})
        }
    ]

instance LogicalModel AuctionProp where
    logic = abstractionLogic @AuctionModel

instance HasLogicalModel AuctionProp AuctionModel where
    satisfiesProperty (Selling p) am = satisfiesProperty p (selling am)
    satisfiesProperty (CurrentBid p) am = satisfiesProperty p (currentBid am)

instance HasPermutationGenerator AuctionProp AuctionModel where
  generators = abstractionMorphisms

instance HasParameterisedGenerator AuctionProp AuctionModel where
  parameterisedGenerator = buildGen baseAuctionGen


baseAuctionGen :: Gen AuctionModel
baseAuctionGen =
  AuctionModel
    <$> genSatisfying @SingletonValueProp Yes
    <*> genSatisfying @BidProp Yes

spec :: Spec
spec = do
    fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: BidModel :+ BidProp) "bid generator" Yes
    xdescribe "bidModelTests" $
      mapM_ fromHedgehogGroup $
        permutationGeneratorSelfTest
          True
          (\(_ :: Morphism BidProp bid) -> True)
          baseBidGen
    fromHedgehogGroup $ runGeneratorTestsWhere (Apropos :: AuctionModel :+ AuctionProp) "auction generator" Yes
    xdescribe "auctionModelTests" $ do
      mapM_ fromHedgehogGroup $
        permutationGeneratorSelfTest
          True
          (\(_ :: Morphism AuctionProp auction) -> True)
          baseAuctionGen
