module Api.Photos.SignedUrl where

import Prelude

import Api (Client)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Network.HTTP.Affjax (AJAX, URL)

type ResponseOkRec
  = { url :: URL
    }
newtype ResponseOk = ResponseOk ResponseOkRec
derive instance genericResponseOk :: Generic ResponseOk _
instance decodeResponseOk :: Decode ResponseOk where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

create :: forall eff. Client -> String -> Aff (ajax :: AJAX | eff) URL
create cli filename = do
  ResponseOk ok <- Api.post cli path body
  pure ok.url
  where
    path = "/my/photos/signed_url"
    body = toForeign $ { filename }
