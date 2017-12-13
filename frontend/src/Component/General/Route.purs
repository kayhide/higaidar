module Component.General.Route where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Location
  = Home
  | Login

oneSlash :: Match Unit
oneSlash = lit "/"

routing :: Match Location
routing = login <|> home
  where
    home = Home <$ lit ""
    login = Login <$ route "login"

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  Login -> "#/login"
