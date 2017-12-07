module Route where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (int, lit)

data Location
  = Home
  | UsersIndex
  | UsersShow Int

oneSlash :: Match Unit
oneSlash = lit "/"

usersIndex :: Match Location
usersIndex = UsersIndex <$ oneSlash

routing :: Match Location
routing = usersShow <|> usersIndex <|> home
  where
    home = Home <$ lit ""
    usersIndex = UsersIndex <$ route "users"
    usersShow = UsersShow <$> (route "users" *> int)

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  UsersIndex -> "#/users"
  UsersShow i -> "#/users/" <> show i
