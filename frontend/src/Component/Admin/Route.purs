module Component.Admin.Route where

import AppPrelude

import Control.Alt ((<|>))
import Routing.Match (Match, int, lit)

data Location
  = Home
  | Login
  | UsersIndex
  | UsersShow Int
  | PhotosIndex
  | PestsIndex

oneSlash :: Match Unit
oneSlash = lit "/"

routing :: Match Location
routing = usersShow <|> usersIndex <|> photosIndex <|> pestsIndex <|> login <|> home
  where
    home = Home <$ lit ""
    login = Login <$ route "login"
    usersIndex = UsersIndex <$ route "users"
    usersShow = UsersShow <$> (route "users" *> int)
    photosIndex = PhotosIndex <$ route "photos"
    pestsIndex = PestsIndex <$ route "pests"

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  Login -> "#/login"
  UsersIndex -> "#/users"
  UsersShow i -> "#/users/" <> show i
  PhotosIndex -> "#/photos"
  PestsIndex -> "#/pests"
