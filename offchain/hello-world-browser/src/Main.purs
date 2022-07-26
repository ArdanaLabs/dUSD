module Main
  ( main
  , testMain
  ) where

import Contract.Prelude

import Contract.Monad (configWithLogLevel)
import Cardano.TextEnvelope
  ( TextEnvelopeType
      ( PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , textEnvelopeBytes
  )
import Data.Maybe (Maybe(Nothing,Just))
import Data.Log.Level (LogLevel(Trace))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Exception(error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HelloWorld.AppM (runAppM)
import HelloWorld.Page.Home as Home
import HelloWorld.TestM (runTestM)
import Serialization (privateKeyFromBytes)
import Serialization.Address (NetworkId(TestnetId))
import Wallet(mkKeyWallet,mkNamiWalletAff,Wallet)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let useKeyWallet = true
    wallet <- if useKeyWallet
                then loadKeyWallet
                else mkNamiWalletAff
    contractConfig <- configWithLogLevel TestnetId wallet Trace
    let
      store =
        { contractConfig
        , lastOutput: Nothing
        }
    rootComponent <- runAppM store Home.component
    runUI rootComponent unit body

loadKeyWallet :: Aff Wallet
loadKeyWallet = do
  let keyStr   = "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"PaymentSigningKey\",\"cborHex\":\"5820d071b5fc9e6f8d1cbc0a4b22dd2ce4fb8f537d0bfe3e6073a758e65c1591275e\"}"
  let stakeStr = "{\"type\":\"StakeSigningKeyShelley_ed25519\",\"description\":\"StakeSigningKey\",\"cborHex\":\"58204b43bbce308317030a526ae92f9579ecafebaa0da20fa46ddc48e12e07de6472\"}"
  keyBytes <- textEnvelopeBytes keyStr PaymentSigningKeyShelleyed25519
  stakeBytes <- textEnvelopeBytes stakeStr StakeSigningKeyShelleyed25519
  privateKey <- liftM (error "Unable to decode private payment key")
    $ PrivatePaymentKey <$> privateKeyFromBytes (wrap keyBytes)
  stakingKey <- liftM (error "Unable to decode private stake key")
    $ PrivateStakeKey <$> privateKeyFromBytes (wrap stakeBytes)
  pure $ mkKeyWallet privateKey (Just stakingKey)

testMain :: Effect Unit
testMain =
  HA.runHalogenAff do
    body <- HA.awaitBody

    let rootComponent = H.hoist runTestM Home.component
    runUI rootComponent unit body
