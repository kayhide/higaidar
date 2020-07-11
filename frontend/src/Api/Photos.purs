module Api.Photos where

import AppPrelude

import Api (Client)
import Api as Api
import Model.Photo (Photo, PhotoId, _id)


index :: Client -> Aff (Array Photo)
index cli = Api.get cli path
  where
    path = "/photos"

filter :: Client -> Api.FilteringMap -> Aff (Array Photo)
filter cli params = Api.get cli path
  where
    path = "/photos?" <> Api.buildQuery params

find :: Client -> PhotoId -> Aff Photo
find cli photoId = Api.get cli path
  where
    path = "/photos/" <> show photoId

update :: Client -> Photo -> Aff Photo
update cli photo = Api.patch cli path photo
  where
    path = "/photos/" <> show (photo ^. _id)

destroy :: Client -> PhotoId -> Aff Unit
destroy cli photoId = Api.delete cli path
  where
    path = "/photos/" <> show photoId
