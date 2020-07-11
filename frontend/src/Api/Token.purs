module Api.Token where

import AppPrelude

import Affjax (URL)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Api as Api
import Data.Argonaut (class DecodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Model.User (User)


newtype ResponseOk =
  ResponseOk
  { token :: Api.AuthenticationToken
  , user :: User
  }

derive instance newtypeToken :: Newtype ResponseOk _
derive instance genericToken :: Generic ResponseOk _
derive newtype instance decodeJsonToken :: DecodeJson ResponseOk


getToken :: URL -> Api.AuthenticateForm -> Aff ResponseOk
getToken url form = do
  let body = Just $ RequestBody.Json $ encodeJson form
  Affjax.post ResponseFormat.json url body
  >>= Api.handleRequestError
  >>= Api.handle

authenticate :: Api.Client -> Api.AuthenticateForm -> Aff Api.Client
authenticate (Api.Client cli) form = do
  ResponseOk { token, user } <- getToken url form
  pure $ Api.Client $ cli { token = Just token, user = Just user }
  where
    url = cli.endpoint <> "/token"
