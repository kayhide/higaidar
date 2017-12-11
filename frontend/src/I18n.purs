module I18n where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(..))
import Data.Either (hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (maybe)


localizeDateTime :: forall q. Locale -> DateTime -> String
localizeDateTime (Locale _ dur) dt =
  maybe "" id $
    DateTime.adjust (negate dur) dt
    >>= hush <<< formatDateTime "YYYY/MM/DD HH:mm:ss"
