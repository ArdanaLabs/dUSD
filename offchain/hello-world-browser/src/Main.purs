module Main
  ( main
  , testMain
  ) where

import Contract.Prelude

import Contract.Monad (configWithLogLevel)
import Data.Maybe (Maybe(Nothing,Just))
import Data.Log.Level (LogLevel(Trace))
import Effect (Effect)
import Effect.Exception(throw)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import HelloWorld.TestM (runTestM)
import Serialization (privateKeyFromBytes)
import Serialization.Address (NetworkId(TestnetId))
import Wallet(mkKeyWallet,Wallet)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Types.RawBytes(hexToRawBytes)



main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    wallet <- case keyWallet of
      Nothing -> liftEffect $ throw "key wallet failed to decode"
      Just w -> pure w
    contractConfig <- configWithLogLevel TestnetId wallet Trace
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body

keyWallet :: Maybe Wallet
keyWallet = do
  privateKey <- PrivatePaymentKey <$> (privateKeyFromBytes =<< (hexToRawBytes "5820d071b5fc9e6f8d1cbc0a4b22dd2ce4fb8f537d0bfe3e6073a758e65c1591275e"))
  stakingKey <- PrivateStakeKey <$> (privateKeyFromBytes =<< (hexToRawBytes "58204b43bbce308317030a526ae92f9579ecafebaa0da20fa46ddc48e12e07de6472"))
  pure $ mkKeyWallet privateKey (Just stakingKey)

testMain :: Effect Unit
testMain =
  HA.runHalogenAff do
    body <- HA.awaitBody

    let rootComponent = H.hoist runTestM Home.component
    runUI rootComponent unit body
