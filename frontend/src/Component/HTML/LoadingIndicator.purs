module Component.HTML.LoadingIndicator where

import AppPrelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

render :: forall props act. Boolean -> HH.HTML props act
render on =
  HH.div
  [ HP.class_ $ H.ClassName $ "loading-indicator" <> if on then " _on" else "" ]
  [
    HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x text-warning" ] []
  ]
