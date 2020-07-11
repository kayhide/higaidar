module Component.HTML.DateTime where

import AppPrelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime (formatDateTime)
import Data.Time.Duration (negateDuration)
import Halogen.HTML as HH
import Model.DateTime (Locale(..))


render :: forall props act. DateTime -> Locale -> HH.HTML props act
render dt (Locale _ dur) =
  HH.text $ either identity identity $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
  where
    dt_ = (DateTime.adjust (negateDuration dur)) dt
