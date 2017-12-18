module Component.Admin.Route where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (int, lit)

data Location
  = Home
  | Login
  | UsersIndex
  | UsersShow Int
  | PestsIndex

oneSlash :: Match Unit
oneSlash = lit "/"

routing :: Match Location
routing = usersShow <|> usersIndex <|> pestsIndex <|> login <|> home
  where
    home = Home <$ lit ""
    login = Login <$ route "login"
    usersIndex = UsersIndex <$ route "users"
    usersShow = UsersShow <$> (route "users" *> int)
    pestsIndex = PestsIndex <$ route "pests"

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  Login -> "#/login"
  UsersIndex -> "#/users"
  UsersShow i -> "#/users/" <> show i
  PestsIndex -> "#/pests"
