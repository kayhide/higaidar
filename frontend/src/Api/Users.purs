module Api.Users where

import Prelude

import Api.Token (AuthenticationToken)
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Left, Right))
import Data.Foreign.Class (class Decode, encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Model.User (User(..), UserId, Users, _id)
import Network.HTTP.Affjax (AJAX, URL, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))


newtype ResponseNg
  = ResponseNg
    { message :: String
    }
derive instance genericErrorResponse :: Generic ResponseNg _
instance decodeErrorResponse :: Decode ResponseNg where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }


index :: forall eff. URL -> AuthenticationToken -> Aff (ajax :: AJAX | eff) Users
index baseUrl token = do
  res <- get'
  case (runExcept $ decodeJSON res.response) of
    Right users -> pure users
    Left _ -> do
      case (runExcept $ decodeJSON res.response) of
        Right (ResponseNg ng) -> throwError $ error ng.message
        Left err -> throwError <<< error $ res.response <> show err

  where
    url = baseUrl <> "/users"

    get' = affjax $ defaultRequest { url = url, headers = headers }

    headers = [
      RequestHeader "Authorization" $ "Bearer " <> token
    ]

find :: forall eff. URL -> AuthenticationToken -> UserId -> Aff (ajax :: AJAX | eff) User
find baseUrl token userId = do
  res <- get'
  case (runExcept $ decodeJSON res.response) of
    Right user -> pure user
    Left _ -> do
      case (runExcept $ decodeJSON res.response) of
        Right (ResponseNg ng) -> throwError $ error ng.message
        Left err -> throwError <<< error $ res.response <> show err

  where
    get' = affjax $ defaultRequest { url = url, headers = headers }

    url = baseUrl <> "/users/" <> show userId
    headers = [
      RequestHeader "Authorization" $ "Bearer " <> token
    ]

update :: forall eff. URL -> AuthenticationToken -> User -> Aff (ajax :: AJAX | eff) User
update baseUrl token user = do
  res <- patch'
  case (runExcept $ decodeJSON res.response) of
    Right user -> pure user
    Left _ -> do
      case (runExcept $ decodeJSON res.response) of
        Right (ResponseNg ng) -> throwError $ error ng.message
        Left err -> throwError <<< error $ res.response <> show err

  where
    patch' = affjax $ defaultRequest { method = Left PATCH
                                     , url = url
                                     , headers = headers
                                     , content = Just $ encodeJSON user }

    url = baseUrl <> "/users/" <> show (user ^. _id)
    headers = [
      RequestHeader "Authorization" $ "Bearer " <> token
    ]
