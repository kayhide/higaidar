module MainGeneral where

import AppPrelude

import AppConfig (readConfig)
import Component.General.Layout as Layout
import Component.General.Route as R
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches)


main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    appConfig <- readConfig
    driver <- runUI Layout.ui appConfig body
    liftEffect $ do
      matches R.routing \ _ new ->
        launchAff_ $ driver.query $ H.tell $ Layout.Goto new
