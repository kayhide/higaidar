module Model.User where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readString, tagOf)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (parse, toDateTime)
import Data.List.NonEmpty as NEL
import Data.Maybe (maybe)


newtype User =
  User
  { id :: Int
  , code :: Int
  , tel :: String
  , name :: String
  , created_at :: DateTime
  , updated_at :: DateTime
  }


derive instance genericPhoto :: Generic User _
instance showPhoto :: Show User where
  show = genericShow

instance decodePhoto :: Decode User where
  decode v = do
    id <- decode =<< v ! "id"
    code <- decode =<< v ! "code"
    tel <- decode =<< v ! "tel"
    name <- decode =<< v ! "name"
    created_at <- readDateTime =<< v ! "created_at"
    updated_at <- readDateTime =<< v ! "updated_at"
    pure $ User { id, code, tel, name, created_at, updated_at }


type Users = Array User


readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromString) $ readString value
  where
    fromString = maybe error pure <<< toDateTime <<< unsafePerformEff <<< parse
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)
