module Tripper.Feature.User.Capability where

import Tripper.Prelude
import Tripper.Feature.User.Types

import Tripper.Feature.Auth.Capability (CurrentUser)
import Halogen (HalogenM)

class MonadAff m <= ManageUser m where
  getMe    :: m (Maybe CurrentUser)
  getUsers :: m (Either Error (Array User))

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots out m) where
  getMe    = lift getMe
  getUsers = lift getUsers