module Main where

import Tripper.Prelude

import Halogen (hoist, tell)
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Tripper.AppM (runAppM)
import Tripper.App.Routes (routeCodec)
import Tripper.Config (Config (..), acquireConfig)
import Routing.PushState (matchesWith)
import Routing.Duplex (parse)

import Tripper.App.Main as Main

main :: Effect Unit
main = runHalogenAff do
  body   <- awaitBody
  config <- liftEffect acquireConfig

  let 
    rootComponent = hoist (runAppM config) Main.component
    Config { nav } = config

  root <- runUI rootComponent unit body

  void $ liftEffect $ nav # matchesWith (parse routeCodec) \old new ->
    launchAff_ $ root.query $ tell $ Main.Navigate new
    

