module Model.Pest where

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


type PestId = Int

newtype Pest =
  Pest
  { id :: PestId
  , label :: String
  , created_at :: DateTime
  , updated_at :: DateTime
  }

_id :: Lens' Pest PestId
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_label :: Lens' Pest String
_label = _Newtype <<< prop (SProxy :: SProxy "label")

_created_at :: Lens' Pest DateTime
_created_at = _Newtype <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' Pest DateTime
_updated_at = _Newtype <<< prop (SProxy :: SProxy "updated_at")


derive instance newtypePest :: Newtype Pest _
derive instance genericPest :: Generic Pest _
instance showPest :: Show Pest where
  show = genericShow

instance encodeJsonPest :: EncodeJson Pest where
  encodeJson (Pest pest) =
    encodeJson
    <<< Record.modify (SProxy :: _ "created_at") encodeDateTime
    <<< Record.modify (SProxy :: _ "updated_at") encodeDateTime
    $ pest

instance decodeJsonPest :: DecodeJson Pest where
  decodeJson json = do
    obj <- decodeJson json
    createdAt <- decodeDateTime obj.created_at
    updatedAt <- decodeDateTime obj.updated_at
    pure $ wrap
      <<< Record.modify (SProxy :: _ "created_at") (const createdAt)
      <<< Record.modify (SProxy :: _ "updated_at") (const updatedAt)
      $ obj



toEntity :: Pest -> PestEntity
toEntity (Pest { label }) = PestEntity { label }


newtype PestEntity =
  PestEntity
  { label :: String
  }

derive instance newtypePestEntity :: Newtype PestEntity _
derive instance genericPestEntity :: Generic PestEntity _
instance showPestEntity :: Show PestEntity where
  show = genericShow

derive newtype instance encodeJsonPestEntity :: EncodeJson PestEntity
