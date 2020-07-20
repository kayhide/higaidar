module Model.User where

import AppPrelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Model.DateTime (decodeDateTime, encodeDateTime)
import Record as Record


type UserId = Int

newtype User =
  User
  { id :: UserId
  , code :: String
  , tel :: String
  , name :: String
  , is_admin :: Boolean
  , is_editor :: Boolean
  , created_at :: DateTime
  , updated_at :: DateTime
  }

_id :: Lens' User UserId
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_code :: Lens' User String
_code = _Newtype <<< prop (SProxy :: SProxy "code")

_tel :: Lens' User String
_tel = _Newtype <<< prop (SProxy :: SProxy "tel")

_name :: Lens' User String
_name = _Newtype <<< prop (SProxy :: SProxy "name")

_is_admin :: Lens' User Boolean
_is_admin = _Newtype <<< prop (SProxy :: SProxy "is_admin")

_is_editor :: Lens' User Boolean
_is_editor = _Newtype <<< prop (SProxy :: SProxy "is_editor")

_created_at :: Lens' User DateTime
_created_at = _Newtype <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' User DateTime
_updated_at = _Newtype <<< prop (SProxy :: SProxy "updated_at")


derive instance eqUser :: Eq User
derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _
instance showUser :: Show User where
  show = genericShow

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user) =
    encodeJson
    <<< Record.modify (SProxy :: _ "created_at") encodeDateTime
    <<< Record.modify (SProxy :: _ "updated_at") encodeDateTime
    $ user

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    createdAt <- decodeDateTime obj.created_at
    updatedAt <- decodeDateTime obj.updated_at
    pure $ wrap
      <<< Record.modify (SProxy :: _ "created_at") (const createdAt)
      <<< Record.modify (SProxy :: _ "updated_at") (const updatedAt)
      $ obj

type Users = Array User


toEntity :: User -> UserEntity
toEntity (User { code, tel, name, is_admin }) = UserEntity { code, tel, name, is_admin }


newtype UserEntity =
  UserEntity
  { code :: String
  , tel :: String
  , name :: String
  , is_admin :: Boolean
  }

derive instance newtypeUserEntity :: Newtype UserEntity _
derive instance genericUserEntity :: Generic UserEntity _
instance showUserEntity :: Show UserEntity where
  show = genericShow

derive newtype instance encodeJsonUserEntity :: EncodeJson UserEntity
