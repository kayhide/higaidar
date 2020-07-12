module Model.Crop where

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


type CropId = Int

newtype Crop =
  Crop
  { id :: CropId
  , label :: String
  , created_at :: DateTime
  , updated_at :: DateTime
  }

_id :: Lens' Crop CropId
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_label :: Lens' Crop String
_label = _Newtype <<< prop (SProxy :: SProxy "label")

_created_at :: Lens' Crop DateTime
_created_at = _Newtype <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' Crop DateTime
_updated_at = _Newtype <<< prop (SProxy :: SProxy "updated_at")


derive instance newtypeCrop :: Newtype Crop _
derive instance genericCrop :: Generic Crop _
instance showCrop :: Show Crop where
  show = genericShow

instance encodeJsonCrop :: EncodeJson Crop where
  encodeJson (Crop pest) =
    encodeJson
    <<< Record.modify (SProxy :: _ "created_at") encodeDateTime
    <<< Record.modify (SProxy :: _ "updated_at") encodeDateTime
    $ pest

instance decodeJsonCrop :: DecodeJson Crop where
  decodeJson json = do
    obj <- decodeJson json
    createdAt <- decodeDateTime obj.created_at
    updatedAt <- decodeDateTime obj.updated_at
    pure $ wrap
      <<< Record.modify (SProxy :: _ "created_at") (const createdAt)
      <<< Record.modify (SProxy :: _ "updated_at") (const updatedAt)
      $ obj



toEntity :: Crop -> CropEntity
toEntity (Crop { label }) = CropEntity { label }


newtype CropEntity =
  CropEntity
  { label :: String
  }

derive instance newtypeCropEntity :: Newtype CropEntity _
derive instance genericCropEntity :: Generic CropEntity _
instance showCropEntity :: Show CropEntity where
  show = genericShow

derive newtype instance encodeJsonCropEntity :: EncodeJson CropEntity
