module Api where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', lens, (^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Model.User (User)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, URL)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.RequestHeader (RequestHeader(..))


newtype Client
  = Client
    { endpoint :: URL
    , user :: Maybe User
    , token :: Maybe AuthenticationToken
    }

makeClient :: URL -> Client
makeClient endpoint = Client { endpoint, user: Nothing, token: Nothing }

derive instance genericClient :: Generic Client _
derive instance newtypeClient :: Newtype Client _
instance showClient :: Show Client where
  show = genericShow

_endpoint :: Lens' Client URL
_endpoint = lens (_.endpoint <<< unwrap) (\s a -> wrap $ _{ endpoint = a } $ unwrap s)

_user :: Lens' Client (Maybe User)
_user = lens (_.user <<< unwrap) (\s a -> wrap $ _{ user = a } $ unwrap s)

_token :: Lens' Client (Maybe AuthenticationToken)
_token = lens (_.token <<< unwrap) (\s a -> wrap $ _{ token = a } $ unwrap s)


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


isAuthenticated :: Client -> Boolean
isAuthenticated = case _ of
  Client { token: Just _ } -> true
  _ -> false


get :: forall eff a. Decode a => Client -> String -> Aff (ajax :: AJAX | eff) a
get cli path
  = buildGet cli path >>= Affjax.affjax >>= handle

post :: forall eff a b. Encode a => Decode b => Client -> String -> a -> Aff (ajax :: AJAX | eff) b
post cli path x
  = buildPost cli path x >>= Affjax.affjax >>= handle

patch :: forall eff a b. Encode a => Decode b => Client -> String -> a -> Aff (ajax :: AJAX | eff) b
patch cli path x
  = buildPatch cli path x >>= Affjax.affjax >>= handle

verifyToken :: forall eff. Client -> Aff eff AuthenticationToken
verifyToken (Client cli) = maybe throw_ pure cli.token
  where
    throw_ = throwError $ error "Token is not ready."

buildGet :: forall eff. Client -> String -> Aff eff (AffjaxRequest Unit)
buildGet cli path = do
  token <- verifyToken cli
  let url = cli ^. _endpoint <> path
      headers = [ RequestHeader "Authorization" $ "Bearer " <> token ]
  pure $ Affjax.defaultRequest { url = url, headers = headers }

buildPost :: forall eff a. Encode a => Client -> String -> a -> Aff eff (AffjaxRequest String)
buildPost cli path x = do
  req <- buildGet cli path
  pure $ req { method = Left POST, content = Just $ encodeJSON x }

buildPatch :: forall eff a. Encode a => Client -> String -> a -> Aff eff (AffjaxRequest String)
buildPatch cli path x = do
  req <- buildPost cli path x
  pure $ req { method = Left PATCH }


newtype ResponseNg
  = ResponseNg
    { message :: String
    }
derive instance genericErrorResponse :: Generic ResponseNg _
instance decodeErrorResponse :: Decode ResponseNg where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }


handle :: forall eff a. Decode a => AffjaxResponse String -> Aff eff a
handle res = do
  case (runExcept $ decodeJSON res.response) of
    Right x -> pure x
    Left _ -> handleError res.response

handleError :: forall eff a. String -> Aff eff a
handleError body =
  case (runExcept $ decodeJSON body) of
    Right (ResponseNg ng) -> throwError $ error ng.message
    Left err -> throwError <<< error $ body <> show err
