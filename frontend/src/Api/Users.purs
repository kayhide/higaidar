module Api.Users where

import Prelude

import Api.Token (AuthenticationToken)
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Left, Right))
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Model.User (Users)
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
