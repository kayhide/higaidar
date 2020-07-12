module Api.Crops where

import AppPrelude

import Api (Client)
import Api as Api
import Model.Crop (Crop, CropEntity, CropId, _id)


index :: Client -> Aff (Array Crop)
index cli = Api.get cli path
  where
    path = "/crops"

find :: Client -> CropId -> Aff Crop
find cli cropId = Api.get cli path
  where
    path = "/crops/" <> show cropId

create :: Client -> CropEntity -> Aff Crop
create cli crop = Api.post cli path crop
  where
    path = "/crops"

update :: Client -> Crop -> Aff Crop
update cli crop = Api.patch cli path crop
  where
    path = "/crops/" <> show (crop ^. _id)

destroy :: Client -> CropId -> Aff Unit
destroy cli cropId = Api.delete cli path
  where
    path = "/crops/" <> show cropId
