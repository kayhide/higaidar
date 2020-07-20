module Api.My.Photos where

import AppPrelude

import Api.Client (Client)
import Api as Api
import Model.Photo (Photo, PhotoId, _id)


index :: Client -> Aff (Array Photo)
index cli = Api.get cli path
  where
    path = "/my/photos"

find :: Client -> PhotoId -> Aff Photo
find cli photoId = Api.get cli path
  where
    path = "/my/photos/" <> show photoId

update :: Client -> Photo -> Aff Photo
update cli photo = Api.patch cli path photo
  where
    path = "/my/photos/" <> show (photo ^. _id)

destroy :: Client -> PhotoId -> Aff Unit
destroy cli photoId = Api.delete cli path
  where
    path = "/my/photos/" <> show photoId
