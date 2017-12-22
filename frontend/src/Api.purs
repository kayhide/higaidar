module Api where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', lens, (^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Model.User (User)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, URL)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (responseHeaderName, responseHeaderValue)
import Text.Parsing.Simple (parse)
import Text.Parsing.Simple as P


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


type UserCode = String
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


type Range =
  { first :: Int
  , last :: Int
  , count :: Int
  }

type WithRange a =
  { body :: a
  , range :: Range
  }

get :: forall eff a. Decode a => Client -> String -> Aff (ajax :: AJAX | eff) a
get cli path
  = buildGet cli path >>= Affjax.affjax >>= handle

getWithRange :: forall eff a. Decode a => Client -> String -> Aff (ajax :: AJAX | eff) (WithRange a)
getWithRange cli path = do
  res <- buildGet cli path >>= Affjax.affjax
  body <- handle res
  range <- pickContentRange res
  pure { body, range }


post :: forall eff a b. Encode a => Decode b => Client -> String -> a -> Aff (ajax :: AJAX | eff) b
post cli path x
  = buildPost cli path x >>= Affjax.affjax >>= handle

patch :: forall eff a b. Encode a => Decode b => Client -> String -> a -> Aff (ajax :: AJAX | eff) b
patch cli path x
  = buildPatch cli path x >>= Affjax.affjax >>= handle

delete :: forall eff. Client -> String -> Aff (ajax :: AJAX | eff) Unit
delete cli path
  = buildDelete cli path >>= Affjax.affjax >>= handle_


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

buildDelete :: forall eff. Client -> String -> Aff eff (AffjaxRequest Unit)
buildDelete cli path = do
  req <- buildGet cli path
  pure $ req { method = Left DELETE }


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

handle_ :: forall eff. AffjaxResponse String -> Aff eff Unit
handle_ res = do
  case String.null res.response of
    true -> pure unit
    false -> handleError res.response

handleError :: forall eff a. String -> Aff eff a
handleError body =
  case (runExcept $ decodeJSON body) of
    Right (ResponseNg ng) -> throwError $ error ng.message
    Left err -> throwError <<< error $ body <> show err

pickContentRange :: forall eff. AffjaxResponse String -> Aff eff Range
pickContentRange res = do
  contentRange <-
    maybe (throwError $ error "Content-Range not present") (pure <<< responseHeaderValue)
    $ Array.find (eq "content-range" <<< responseHeaderName) res.headers
  range <-
    either (const $ throwError $ error "Content-Range mal-formatted") pure
    $ parse parser contentRange
  pure range
  where
    parser = do
      first <- P.int
      void $ P.char '-'
      last <- P.int
      void $ P.char '/'
      count <- P.int
      pure { first, last, count }
