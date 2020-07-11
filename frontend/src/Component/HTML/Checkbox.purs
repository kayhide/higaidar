module Component.HTML.Checkbox where

import AppPrelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall props act. String -> String -> Boolean -> (Boolean -> act) -> HH.HTML props act
render key label value handle =
  HH.div
  [ HP.class_ $ H.ClassName "form-check" ]
  [
    HH.label
    [ HP.class_ $ H.ClassName "form-check-label" ]
    [
      HH.input
       [ HP.class_ $ H.ClassName "form-check-input mr-2"
       , HP.type_ InputCheckbox
       , HP.id_ key
       , HP.checked value
       , HE.onChecked $ Just <<< handle
       ]
     , HH.text label
     ]
  ]
