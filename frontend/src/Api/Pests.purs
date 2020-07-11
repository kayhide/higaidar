module Api.Pests where

import AppPrelude

import Api (Client)
import Api as Api
import Model.Pest (Pest, PestEntity, PestId, _id)


index :: Client -> Aff (Array Pest)
index cli = Api.get cli path
  where
    path = "/pests"

find :: Client -> PestId -> Aff Pest
find cli pestId = Api.get cli path
  where
    path = "/pests/" <> show pestId

create :: Client -> PestEntity -> Aff Pest
create cli pest = Api.post cli path pest
  where
    path = "/pests"

update :: Client -> Pest -> Aff Pest
update cli pest = Api.patch cli path pest
  where
    path = "/pests/" <> show (pest ^. _id)

destroy :: Client -> PestId -> Aff Unit
destroy cli pestId = Api.delete cli path
  where
    path = "/pests/" <> show pestId
