module Api.Token where

import AppPrelude

import Affjax (URL)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Api as Api
import Api.Client (AuthenticateForm, AuthenticationToken, Client(..))
import Data.Argonaut (class DecodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Model.User (User)


newtype ResponseOk =
  ResponseOk
  { token :: AuthenticationToken
  , user :: User
  }

derive instance newtypeToken :: Newtype ResponseOk _
derive instance genericToken :: Generic ResponseOk _
derive newtype instance decodeJsonToken :: DecodeJson ResponseOk


getToken :: URL -> AuthenticateForm -> Aff ResponseOk
getToken url form = do
  let body = Just $ RequestBody.Json $ encodeJson form
  Affjax.post ResponseFormat.json url body
  >>= Api.handleRequestError
  >>= Api.handle

authenticate :: Client -> AuthenticateForm -> Aff Client
authenticate (Client cli) form = do
  ResponseOk { token, user } <- getToken url form
  pure $ Client $ cli { token = Just token, user = Just user }
  where
    url = cli.endpoint <> "/token"
