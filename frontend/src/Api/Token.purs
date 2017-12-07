module Api.Token where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Left, Right))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Network.HTTP.Affjax (AJAX, URL)
import Network.HTTP.Affjax as Affjax

type UserCode = Int
type UserTel = String
type AuthenticationToken = String

newtype AuthenticateForm
  = AuthenticateForm
    { code :: UserCode
    , tel :: UserTel
    }
derive instance genericAuthenticateForm :: Generic AuthenticateForm _

instance showAuthenticateForm :: Show AuthenticateForm where
  show = genericShow

instance encodeAuthenticateForm :: Encode AuthenticateForm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseOk
  = ResponseOk
    { token :: AuthenticationToken
    }
derive instance genericToken :: Generic ResponseOk _
instance decodeToken :: Decode ResponseOk where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype ResponseNg
  = ResponseNg
    { message :: String
    }
derive instance genericErrorResponse :: Generic ResponseNg _
instance decodeErrorResponse :: Decode ResponseNg where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

authenticate :: forall eff. URL -> UserCode -> UserTel -> Aff (ajax :: AJAX | eff) AuthenticationToken
authenticate url code tel = do
  res <- Affjax.post url $ encodeJSON $ AuthenticateForm { code, tel }
  case (runExcept $ decodeJSON res.response) of
    Right (ResponseOk ok) -> pure ok.token
    Left _ -> do
      case (runExcept $ decodeJSON res.response) of
        Right (ResponseNg ng) -> throwError $ error ng.message
        Left err -> throwError <<< error $ res.response <> show err
