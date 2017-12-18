module Model.Photo where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readNullOrUndefined, readString, tagOf, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (parse, toDateTime)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Model.Null (null)
import Model.Photo (PhotoId)
import Network.HTTP.Affjax (URL)

type PhotoId = Int

type PhotoRec
  = { id :: PhotoId
    , user_id :: PhotoId
    , original_url :: URL
    , thumbnail_url :: Maybe URL
    , pest :: Maybe String
    , created_at :: DateTime
    , updated_at :: DateTime
    }
newtype Photo = Photo PhotoRec

_Photo :: Lens' Photo PhotoRec
_Photo = lens (\(Photo r) -> r) (\_ r -> Photo r)

_id :: Lens' Photo PhotoId
_id = _Photo <<< prop (SProxy :: SProxy "id")

_user_id :: Lens' Photo PhotoId
_user_id = _Photo <<< prop (SProxy :: SProxy "user_id")

_original_url :: Lens' Photo String
_original_url = _Photo <<< prop (SProxy :: SProxy "original_url")

_thumbnail_url :: Lens' Photo (Maybe String)
_thumbnail_url = _Photo <<< prop (SProxy :: SProxy "thumbnail_url")

_pest :: Lens' Photo (Maybe String)
_pest = _Photo <<< prop (SProxy :: SProxy "pest")

_created_at :: Lens' Photo DateTime
_created_at = _Photo <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' Photo DateTime
_updated_at = _Photo <<< prop (SProxy :: SProxy "updated_at")


derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow

instance decodePhoto :: Decode Photo where
  decode v = do
    id <- decode =<< v ! "id"
    user_id <- decode =<< v ! "user_id"
    original_url <- decode =<< v ! "original_url"
    thumbnail_url <- traverse decode =<< readNullOrUndefined =<< v ! "thumbnail_url"
    pest <- traverse decode =<< readNullOrUndefined =<< v ! "pest"
    created_at <- readDateTime =<< v ! "created_at"
    updated_at <- readDateTime =<< v ! "updated_at"
    pure $ Photo { id, user_id, original_url, thumbnail_url, pest, created_at, updated_at }


readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromString) $ readString value
  where
    fromString = maybe error pure <<< toDateTime <<< unsafePerformEff <<< parse
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)

instance encodePhoto :: Encode Photo where
  encode = encode <<< toEntity

toEntity :: Photo -> PhotoEntity
toEntity (Photo { original_url, thumbnail_url, pest }) = PhotoEntity { original_url, thumbnail_url, pest }


type PhotoEntityRec
  = { original_url :: URL
    , thumbnail_url :: Maybe URL
    , pest :: Maybe String
    }
newtype PhotoEntity = PhotoEntity PhotoEntityRec

derive instance genericPhotoEntity :: Generic PhotoEntity _
instance showPhotoEntity :: Show PhotoEntity where
  show = genericShow

instance encodePhotoEntity :: Encode PhotoEntity where
  encode (PhotoEntity { original_url, thumbnail_url, pest }) =
    toForeign { original_url
              , thumbnail_url: maybe null encode thumbnail_url
              , pest: maybe null encode pest
              }
