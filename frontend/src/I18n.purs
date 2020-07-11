module I18n where

import AppPrelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime (formatDateTime)
import Data.Time.Duration (negateDuration)
import Model.DateTime (Locale(..))


localizeDateTime :: Locale -> DateTime -> String
localizeDateTime (Locale _ dur) dt =
  fromMaybe "" $
    DateTime.adjust (negateDuration dur) dt
    >>= hush <<< formatDateTime "YYYY/MM/DD HH:mm:ss"
