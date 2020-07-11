module Model.DateTime where

import AppPrelude

import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.RFC3339String (RFC3339String(..), fromDateTime, toDateTime)
import Data.Time.Duration (Minutes(..))


encodeDateTime :: DateTime -> Json
encodeDateTime dt = do
  let str = unwrap $ fromDateTime dt
  encodeJson str

decodeDateTime :: Json -> Either String DateTime
decodeDateTime json = do
  str <- decodeJson json
  let decodeError = "Could not decode DateTime from " <> str
  note decodeError $ toDateTime $ RFC3339String str


newtype Timezone = Timezone Minutes
derive instance newtypeTimezone :: Newtype Timezone _
derive instance eqTimezone :: Eq Timezone
derive instance ordTimezone :: Ord Timezone


getTimezone :: Effect Timezone
getTimezone = do
  offset <- JSDate.getTimezoneOffset =<< JSDate.now
  pure $ Timezone $ Minutes offset


data Locale = Locale (Maybe String) Minutes

getLocale :: Effect Locale
getLocale = do
  offset <- JSDate.getTimezoneOffset =<< JSDate.now
  pure $ Locale Nothing $ Minutes offset
