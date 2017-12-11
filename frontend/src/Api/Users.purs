module Api.Users where

import Prelude

import Api (Client)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Lens ((^.))
import Model.User (User, UserId, Users, _id)
import Network.HTTP.Affjax (AJAX)


index :: forall eff. Client -> Aff (ajax :: AJAX | eff) Users
index cli = Api.get cli path
  where
    path = "/users"

find :: forall eff. Client -> UserId -> Aff (ajax :: AJAX | eff) User
find cli userId = Api.get cli path
  where
    path = "/users/" <> show userId

update :: forall eff. Client -> User -> Aff (ajax :: AJAX | eff) User
update cli user = Api.patch cli path user
  where path = "/users/" <> show (user ^. _id)
