module Component.HTML.LoadingIndicator where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

render :: forall q. Boolean -> H.ComponentHTML q
render on =
  HH.div
  [ HP.class_ $ H.ClassName $ "loading-indicator" <> if on then " _on" else "" ]
  [
    HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x" ] []
  ]
