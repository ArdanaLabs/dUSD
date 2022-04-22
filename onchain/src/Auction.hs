module Auction (auctionContract) where

import Plutarch.Prelude
import Plutarch.Api.V1

auctionContract :: Term s (PDatum :--> PDatum :--> PDatum :--> PUnit)
auctionContract = undefined
