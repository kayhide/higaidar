module Api.Users where

import AppPrelude

import Api (Client, WithRange)
import Api as Api
import Data.Array as Array
import Data.String as String
import Model.User (User, UserEntity, UserId, _id)


index :: Client -> Aff (Array User)
index cli = Api.get cli path
  where
    path = "/users"

page :: Client -> Int -> Int -> Aff (WithRange (Array User))
page cli offset limit = Api.getWithRange cli path
  where
    query = Array.intercalate "&" $ Array.catMaybes
            [ if offset > 0 then Just ("offset=" <> show offset) else Nothing
            , if limit > 0 then Just ("limit=" <> show limit) else Nothing
            ]
    path = if String.null query then "/users" else "/users?" <> query

find :: Client -> UserId -> Aff User
find cli userId = Api.get cli path
  where
    path = "/users/" <> show userId

some :: Client -> Array UserId -> Aff (Array User)
some cli ids = Api.get cli path
  where
    path = "/users?id=in(" <> (Array.intercalate "," $ show <$> ids) <> ")"


create :: Client -> UserEntity -> Aff User
create cli user = Api.post cli path user
  where
    path = "/users"

update :: Client -> User -> Aff User
update cli user = Api.patch cli path user
  where
    path = "/users/" <> show (user ^. _id)

destroy :: Client -> UserId -> Aff Unit
destroy cli userId = Api.delete cli path
  where
    path = "/users/" <> show userId
