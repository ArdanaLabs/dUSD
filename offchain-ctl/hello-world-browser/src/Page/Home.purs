module Page.Home where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
