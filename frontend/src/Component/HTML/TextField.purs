module Component.HTML.TextField where

import Prelude

import Data.Lens (Iso', Lens, Lens', Setter', _Just, assign, iso, lens, set, to, view, (^.))
import Halogen (Action)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall a q.
          String
          -> String
          -> String
          -> (String -> Action q)
          -> H.ComponentHTML q
render key label value query =
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
    , HE.onValueInput $ HE.input query
    ]
  ]