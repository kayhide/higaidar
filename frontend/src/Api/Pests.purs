module Api.Pests where

import Prelude

import Api (Client)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Lens ((^.))
import Model.Pest (Pest, PestEntity, PestId, _id)
import Network.HTTP.Affjax (AJAX)


index :: forall eff. Client -> Aff (ajax :: AJAX | eff) (Array Pest)
index cli = Api.get cli path
  where
    path = "/pests"

find :: forall eff. Client -> PestId -> Aff (ajax :: AJAX | eff) Pest
find cli pestId = Api.get cli path
  where
    path = "/pests/" <> show pestId

create :: forall eff. Client -> PestEntity -> Aff (ajax :: AJAX | eff) Pest
create cli pest = Api.post cli path pest
  where
    path = "/pests"

update :: forall eff. Client -> Pest -> Aff (ajax :: AJAX | eff) Pest
update cli pest = Api.patch cli path pest
  where
    path = "/pests/" <> show (pest ^. _id)

destroy :: forall eff. Client -> PestId -> Aff (ajax :: AJAX | eff) Unit
destroy cli pestId = Api.delete cli path
  where
    path = "/pests/" <> show pestId
