module Api.Token where

import Prelude

import Api as Api
import Control.Monad.Aff (Aff)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Model.User (User)
import Network.HTTP.Affjax (AJAX, URL)
import Network.HTTP.Affjax as Affjax

type ResponseOkRec
  = { token :: Api.AuthenticationToken
    , user :: User
    }
newtype ResponseOk = ResponseOk ResponseOkRec
derive instance genericToken :: Generic ResponseOk _
instance decodeToken :: Decode ResponseOk where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }


getToken :: forall eff. URL -> Api.AuthenticateForm -> Aff (ajax :: AJAX | eff) ResponseOk
getToken url form = do
  res <- Affjax.post url $ encodeJSON $ form
  Api.handle res

authenticate :: forall eff. Api.Client -> Api.AuthenticateForm -> Aff (ajax :: AJAX | eff) Api.Client
authenticate (Api.Client cli) form = do
  ResponseOk { token, user } <- getToken url form
  pure $ Api.Client $ cli { token = Just token, user = Just user }
  where
    url = cli.endpoint <> "/token"
