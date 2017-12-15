module Api.My.Photos where

import Prelude

import Api (Client)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Lens ((^.))
import Model.Photo (Photo, PhotoId, _id)
import Network.HTTP.Affjax (AJAX)


index :: forall eff. Client -> Aff (ajax :: AJAX | eff) (Array Photo)
index cli = Api.get cli path
  where
    path = "/my/photos"

find :: forall eff. Client -> PhotoId -> Aff (ajax :: AJAX | eff) Photo
find cli photoId = Api.get cli path
  where
    path = "/my/photos/" <> show photoId

update :: forall eff. Client -> Photo -> Aff (ajax :: AJAX | eff) Photo
update cli photo = Api.patch cli path photo
  where
    path = "/my/photos/" <> show (photo ^. _id)

destroy :: forall eff. Client -> PhotoId -> Aff (ajax :: AJAX | eff) Unit
destroy cli photoId = Api.delete cli path
  where
    path = "/my/photos/" <> show photoId
