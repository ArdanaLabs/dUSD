module Main
  ( main
  ) where

import Api
  (helloScript
  ,sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  ,datumLookup
  )
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Contract.Prelude
import Contract.Config(testnetConfig)
import Contract.Log (logInfo')
import Contract.Scripts (validatorHash)
import Contract.Monad
  ( runContract
  , liftContractAffM
  )
import Test.Spec.Assertions (shouldEqual)
import Wallet.Spec(WalletSpec(ConnectToGero))


data Action
  = Init
  | Incr

type State = Int

component
  :: forall q o m
   . MonadAff m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const 0
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Init -> do
      H.modify_ \_ -> 0
    Incr -> do
      H.modify_ \cnt -> cnt + 1

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render count =
    HH.main_
      [ HH.div
          [ HP.id "counter" ]
          [ HH.text $ show count
          ]
      , HH.button
          [ HP.id "initialize"
          , HE.onClick \_ -> Init
          ]
          [ HH.text "Initialize"
          ]
      , HH.button
          [ HP.id "increment"
          , HE.onClick \_ -> Incr
          ]
          [ HH.text "Increment"
          ]
      ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    _ <- runUI component unit body
    let cfg = testnetConfig{walletSpec = Just ConnectToGero}
    void $ runContract cfg $ do
      let param = 4
      let init = 3
      logInfo' "Full integrationTest"
      validator <- helloScript param
      vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
      ti1 <- sendDatumToScript init vhash
      datum1 <- datumLookup ti1
      datum1 `shouldEqual` init
      ti2 <- setDatumAtScript (init + param) vhash validator ti1
      datum2 <- datumLookup ti2
      datum2 `shouldEqual` (init + param)
      redeemFromScript vhash validator ti2

