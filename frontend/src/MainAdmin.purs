module MainAdmin where

import Prelude

import Component.Admin.Layout (AppConfig)
import Component.Admin.Layout as Layout
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Dom.Meta (META)
import Dom.Meta as Meta
import Dom.Storage (STORAGE)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)


type AppEffs = HA.HalogenEffects (meta :: META, ajax :: AJAX, now :: NOW, storage :: STORAGE, console :: CONSOLE)

main :: Eff AppEffs Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  appConfig <- liftEff readConfig
  driver <- runUI Layout.ui appConfig body
  liftEff $ Layout.matchRoute driver

readConfig :: Eff AppEffs AppConfig
readConfig = do
  stage <- Meta.get "STAGE"
  apiEndpoint <- Meta.get "API_ENDPOINT"
  pure $ { stage, apiEndpoint }
