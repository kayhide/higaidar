module Api.Photos.SignedUrl where

import AppPrelude

import Affjax (URL)
import Api (Client)
import Api as Api
import Data.Argonaut (class DecodeJson, encodeJson)


newtype ResponseOk =
  ResponseOk
  { url :: URL
  }

derive instance newtypeResponseOk :: Newtype ResponseOk _
derive newtype instance decodeJsonResponseOk :: DecodeJson ResponseOk

create :: Client -> String -> Aff URL
create cli filename = do
  ResponseOk ok <- Api.post cli path body
  pure ok.url
  where
    path = "/my/photos/signed_url"
    body = encodeJson { filename }
