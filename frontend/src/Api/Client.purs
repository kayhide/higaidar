module Api.Client where

import AppPrelude

import Data.Argonaut (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Model.User (User(..))


newtype Client
  = Client
    { endpoint :: String
    , user :: Maybe User
    , token :: Maybe AuthenticationToken
    }

makeClient :: String -> Client
makeClient endpoint = Client { endpoint, user: Nothing, token: Nothing }

derive instance newtypeClient :: Newtype Client _
derive instance genericClient :: Generic Client _
instance showClient :: Show Client where
  show = genericShow

_endpoint :: Lens' Client String
_endpoint = lens (_.endpoint <<< unwrap) (\s a -> wrap $ _{ endpoint = a } $ unwrap s)

_user :: Lens' Client (Maybe User)
_user = lens (_.user <<< unwrap) (\s a -> wrap $ _{ user = a } $ unwrap s)

_token :: Lens' Client (Maybe AuthenticationToken)
_token = lens (_.token <<< unwrap) (\s a -> wrap $ _{ token = a } $ unwrap s)

isAdmin :: Client -> Boolean
isAdmin = case _ of
  Client { user: Just (User { is_admin: true }) } -> true
  _ -> false

isEditor :: Client -> Boolean
isEditor = case _ of
  Client { user: Just (User { is_editor: true }) } -> true
  _ -> false

type UserCode = String
type UserTel = String
type AuthenticationToken = String

newtype AuthenticateForm =
  AuthenticateForm
  { code :: UserCode
  , tel :: UserTel
  }

_code :: Lens' AuthenticateForm UserCode
_code = _Newtype <<< prop (SProxy :: SProxy "code")

_tel :: Lens' AuthenticateForm UserTel
_tel = _Newtype <<< prop (SProxy :: SProxy "tel")

derive instance newtypeAuthenticateForm :: Newtype AuthenticateForm _
derive instance genericAuthenticateForm :: Generic AuthenticateForm _
instance showAuthenticateForm :: Show AuthenticateForm where
  show = genericShow
derive newtype instance encodeJsonAuthenticateForm :: EncodeJson AuthenticateForm


isAuthenticated :: Client -> Boolean
isAuthenticated = case _ of
  Client { token: Just _ } -> true
  _ -> false
