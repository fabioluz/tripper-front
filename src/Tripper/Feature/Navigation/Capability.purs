module Tripper.Feature.Navigation.Capability where

import Tripper.Prelude

import Tripper.App.Routes (Route)
import Routing.PushState (PushStateInterface)
import Halogen (HalogenM)

class HasNav env where
  nav :: env -> PushStateInterface

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate