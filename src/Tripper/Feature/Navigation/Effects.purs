module Tripper.Feature.Navigation.Effects where

import Tripper.Prelude
import Tripper.App.Routes 
import Tripper.Feature.Navigation.Capability

import Foreign (unsafeToForeign)

import Routing.Duplex as RD

navigate :: ∀ env m . MonadEffect m => MonadAsk env m => HasNav env => Route -> m Unit
navigate route = do
  { pushState } <- asks nav
  let state = unsafeToForeign {}
  liftEffect $ pushState state $ RD.print routeCodec route
  