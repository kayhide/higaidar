module AppConfig where

import AppPrelude

import Dom.Meta as Meta


type AppConfig =
  { stage :: String
  , apiEndpoint :: String
  }

readConfig :: forall m. MonadEffect m => m AppConfig
readConfig = liftEffect do
  stage <- Meta.get "STAGE"
  apiEndpoint <- Meta.get "API_ENDPOINT"
  pure $ { stage, apiEndpoint }
