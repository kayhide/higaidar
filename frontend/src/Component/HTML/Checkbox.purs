module Component.HTML.Checkbox where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen (Action)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall q p. String -> String -> Boolean -> (Boolean -> Action q) -> HH.HTML p (q Unit)
render key label value query =
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
       , HE.onChecked $ HE.input query
       ]
     , HH.text label
     ]
  ]
