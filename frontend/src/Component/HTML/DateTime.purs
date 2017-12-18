module Component.HTML.DateTime where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(..))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (maybe)
import Halogen.HTML as HH


render :: forall p i. DateTime -> Locale -> HH.HTML p i
render dt (Locale _ dur) =
  HH.text $ either id id $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
  where
    dt_ = (DateTime.adjust (negate dur)) dt
