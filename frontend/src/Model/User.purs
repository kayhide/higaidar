module Model.User where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readString, tagOf, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (parse, toDateTime)
import Data.Lens (Lens', lens, (^.))
import Data.Lens.Record (prop)
import Data.List.NonEmpty as NEL
import Data.Maybe (maybe)
import Data.Symbol (SProxy(..))

type UserId = Int

type UserRec
  = { id :: UserId
    , code :: Int
    , tel :: String
    , name :: String
    , created_at :: DateTime
    , updated_at :: DateTime
    }
newtype User = User UserRec

_User :: Lens' User UserRec
_User = lens (\(User r) -> r) (\_ r -> User r)

_id :: Lens' User UserId
_id = _User <<< prop (SProxy :: SProxy "id")

_code :: Lens' User Int
_code = _User <<< prop (SProxy :: SProxy "code")

_tel :: Lens' User String
_tel = _User <<< prop (SProxy :: SProxy "tel")

_name :: Lens' User String
_name = _User <<< prop (SProxy :: SProxy "name")

_created_at :: Lens' User DateTime
_created_at = _User <<< prop (SProxy :: SProxy "created_at")

_updated_at :: Lens' User DateTime
_updated_at = _User <<< prop (SProxy :: SProxy "updated_at")


derive instance genericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow

instance decodeUser :: Decode User where
  decode v = do
    id <- decode =<< v ! "id"
    code <- decode =<< v ! "code"
    tel <- decode =<< v ! "tel"
    name <- decode =<< v ! "name"
    created_at <- readDateTime =<< v ! "created_at"
    updated_at <- readDateTime =<< v ! "updated_at"
    pure $ User { id, code, tel, name, created_at, updated_at }

instance encodeUser :: Encode User where
  encode = toForeign <<< toEntity

type Users = Array User


readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromString) $ readString value
  where
    fromString = maybe error pure <<< toDateTime <<< unsafePerformEff <<< parse
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)

toEntity :: User -> UserEntity
toEntity (User { code, tel, name }) = UserEntity { code, tel, name }


type UserEntityRec
  = { code :: Int
    , tel :: String
    , name :: String
    }
newtype UserEntity = UserEntity UserEntityRec

derive instance genericUserEntity :: Generic UserEntity _
instance showUserEntity :: Show UserEntity where
  show = genericShow

instance encodeUserEntity :: Encode UserEntity where
  encode (UserEntity { code, tel, name }) = toForeign { code, tel, name }
