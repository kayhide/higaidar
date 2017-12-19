module Api.Users where

import Prelude

import Api (Client, WithRange)
import Api as Api
import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.String as String
import Model.User (User, UserEntity, UserId, _id)
import Network.HTTP.Affjax (AJAX)


index :: forall eff. Client -> Aff (ajax :: AJAX | eff) (Array User)
index cli = Api.get cli path
  where
    path = "/users"

page :: forall eff. Client -> Int -> Int -> Aff (ajax :: AJAX | eff) (WithRange (Array User))
page cli offset limit = Api.getWithRange cli path
  where
    query = Array.intercalate "&" $ Array.catMaybes
            [ if offset > 0 then Just ("offset=" <> show offset) else Nothing
            , if limit > 0 then Just ("limit=" <> show limit) else Nothing
            ]
    path = if String.null query then "/users" else "/users?" <> query

find :: forall eff. Client -> UserId -> Aff (ajax :: AJAX | eff) User
find cli userId = Api.get cli path
  where
    path = "/users/" <> show userId

create :: forall eff. Client -> UserEntity -> Aff (ajax :: AJAX | eff) User
create cli user = Api.post cli path user
  where
    path = "/users"

update :: forall eff. Client -> User -> Aff (ajax :: AJAX | eff) User
update cli user = Api.patch cli path user
  where
    path = "/users/" <> show (user ^. _id)

destroy :: forall eff. Client -> UserId -> Aff (ajax :: AJAX | eff) Unit
destroy cli userId = Api.delete cli path
  where
    path = "/users/" <> show userId
