module Api.Photos where

import Prelude

import Api (Client)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Lens ((^.))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Global (encodeURI)
import Model.Photo (Photo, PhotoId, _id)
import Network.HTTP.Affjax (AJAX)


index :: forall eff. Client -> Aff (ajax :: AJAX | eff) (Array Photo)
index cli = Api.get cli path
  where
    path = "/photos"

data Filtering a = Eq_ a | In_ (Array a)

querify :: forall a. Show a => Filtering a -> String
querify (Eq_ x) = "eq(" <> (encodeURI $ show x) <> ")"
querify (In_ xs) = "in(" <> (Array.intercalate "," $ map (encodeURI <<< show) xs) <> ")"

filter :: forall eff a. Show a => Client -> StrMap (Filtering a) -> Aff (ajax :: AJAX | eff) (Array Photo)
filter cli params = Api.get cli path
  where
    path = "/photos?" <> query
    query = Array.intercalate "&" $ StrMap.mapWithKey (\k v -> encodeURI k <> "=" <> querify v) params


find :: forall eff. Client -> PhotoId -> Aff (ajax :: AJAX | eff) Photo
find cli photoId = Api.get cli path
  where
    path = "/photos/" <> show photoId

update :: forall eff. Client -> Photo -> Aff (ajax :: AJAX | eff) Photo
update cli photo = Api.patch cli path photo
  where
    path = "/photos/" <> show (photo ^. _id)

destroy :: forall eff. Client -> PhotoId -> Aff (ajax :: AJAX | eff) Unit
destroy cli photoId = Api.delete cli path
  where
    path = "/photos/" <> show photoId
