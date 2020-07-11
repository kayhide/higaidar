module Api.Photos.PresignedPost where

import AppPrelude

import Affjax (URL)
import Api (Client)
import Api as Api
import Data.Argonaut (class DecodeJson, encodeJson)
import Foreign.Object (Object)


newtype ResponseOk =
  ResponseOk
  { url :: URL
  , fields :: Object String
  }


derive instance newtypeResponseOk :: Newtype ResponseOk _
derive newtype instance decodeJsonResponseOk :: DecodeJson ResponseOk

create :: Client -> String -> Aff { url :: URL, fields :: Object String}
create cli filename = do
  ResponseOk ok <- Api.post cli path body
  pure ok
  where
    path = "/my/photos/presigned_post"
    body = encodeJson { filename }
