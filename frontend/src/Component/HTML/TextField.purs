module Component.HTML.TextField where

import AppPrelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall props act. String -> String -> String -> (String -> act) -> HH.HTML props act
render key label value handle =
  HH.div
  [ HP.class_ $ H.ClassName "form-group" ]
  [
    HH.label
    [ HP.for key ]
    [ HH.text label ]
  , HH.input
    [ HP.class_ $ H.ClassName "form-control mr-2"
    , HP.id_ key
    , HP.value value
    , HE.onValueInput $ Just <<< handle
    ]
  ]

renderWithHelp :: forall props act. String -> String -> String -> String -> (String -> act) -> HH.HTML props act
renderWithHelp key label value help handle =
  HH.div
  [ HP.class_ $ H.ClassName "form-group" ]
  [
    HH.label
    [ HP.for key ]
    [ HH.text label ]
  , HH.input
    [ HP.class_ $ H.ClassName "form-control mr-2"
    , HP.id_ key
    , HP.value value
    , HE.onValueInput $ Just <<< handle
    ]
  , HH.small
    [ HP.class_ $ H.ClassName "form-text text-muted" ]
    [ HH.text help ]
  ]
