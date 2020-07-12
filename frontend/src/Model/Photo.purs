module Model.Photo where

import AppPrelude

import Affjax (URL)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Model.DateTime (decodeDateTime, encodeDateTime)
import Model.User (UserId)
import Record as Record


type PhotoId = Int

newtype Photo =
  Photo
  { id :: PhotoId
  , user_id :: UserId
  , original_url :: URL
  , thumbnail_url :: Maybe URL
  , crop :: Maybe String
  , pest :: Maybe String
  , created_at :: DateTime
  , updated_at :: DateTime
  }

_id :: Lens' Photo PhotoId
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_user_id :: Lens' Photo PhotoId
_user_id = _Newtype <<< prop (SProxy :: SProxy "user_id")

_original_url :: Lens' Photo String
_original_url = _Newtype <<< prop (SProxy :: SProxy "original_url")

_thumbnail_url :: Lens' Photo (Maybe String)
_thumbnail_url = _Newtype <<< prop (SProxy :: SProxy "thumbnail_url")

_pest :: Lens' Photo (Maybe String)
_pest = _Newtype <<< prop (SProxy :: SProxy "pest")

_created_at :: Lens' Photo DateTime
_created_at = _Newtype <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' Photo DateTime
_updated_at = _Newtype <<< prop (SProxy :: SProxy "updated_at")


derive instance newtypePhoto :: Newtype Photo _
derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow

instance encodeJsonPhoto :: EncodeJson Photo where
  encodeJson (Photo photo) =
    encodeJson
    <<< Record.modify (SProxy :: _ "created_at") encodeDateTime
    <<< Record.modify (SProxy :: _ "updated_at") encodeDateTime
    $ photo

instance decodeJsonPhoto :: DecodeJson Photo where
  decodeJson json = do
    obj <- decodeJson json
    createdAt <- decodeDateTime obj.created_at
    updatedAt <- decodeDateTime obj.updated_at
    pure $ wrap
      <<< Record.modify (SProxy :: _ "created_at") (const createdAt)
      <<< Record.modify (SProxy :: _ "updated_at") (const updatedAt)
      $ obj


toEntity :: Photo -> PhotoEntity
toEntity (Photo { original_url, thumbnail_url, crop, pest }) =
  PhotoEntity { original_url, thumbnail_url, crop, pest }


newtype PhotoEntity =
  PhotoEntity
  { original_url :: URL
  , thumbnail_url :: Maybe URL
  , crop :: Maybe String
  , pest :: Maybe String
  }

derive instance newtypePhotoEntity :: Newtype PhotoEntity _
derive instance genericPhotoEntity :: Generic PhotoEntity _
instance showPhotoEntity :: Show PhotoEntity where
  show = genericShow

derive newtype instance encodeJsonPhotoEntity :: EncodeJson PhotoEntity
