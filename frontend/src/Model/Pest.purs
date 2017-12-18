module Model.Pest where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readString, tagOf, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (parse, toDateTime)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.List.NonEmpty as NEL
import Data.Maybe (maybe)
import Data.Symbol (SProxy(..))

type PestId = Int

type PestRec
  = { id :: PestId
    , label :: String
    , created_at :: DateTime
    , updated_at :: DateTime
    }
newtype Pest = Pest PestRec

_Pest :: Lens' Pest PestRec
_Pest = lens (\(Pest r) -> r) (\_ r -> Pest r)

_id :: Lens' Pest PestId
_id = _Pest <<< prop (SProxy :: SProxy "id")

_label :: Lens' Pest String
_label = _Pest <<< prop (SProxy :: SProxy "label")

_created_at :: Lens' Pest DateTime
_created_at = _Pest <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' Pest DateTime
_updated_at = _Pest <<< prop (SProxy :: SProxy "updated_at")


derive instance genericPest :: Generic Pest _
instance showPest :: Show Pest where
  show = genericShow

instance decodePest :: Decode Pest where
  decode v = do
    id <- decode =<< v ! "id"
    label <- decode =<< v ! "label"
    created_at <- readDateTime =<< v ! "created_at"
    updated_at <- readDateTime =<< v ! "updated_at"
    pure $ Pest { id, label, created_at, updated_at }

instance encodePest :: Encode Pest where
  encode = toForeign <<< toEntity


readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromString) $ readString value
  where
    fromString = maybe error pure <<< toDateTime <<< unsafePerformEff <<< parse
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)

toEntity :: Pest -> PestEntity
toEntity (Pest { label }) = PestEntity { label }


type PestEntityRec
  = { label :: String
    }
newtype PestEntity = PestEntity PestEntityRec

derive instance genericPestEntity :: Generic PestEntity _
instance showPestEntity :: Show PestEntity where
  show = genericShow

instance encodePestEntity :: Encode PestEntity where
  encode (PestEntity { label }) = toForeign { label }
