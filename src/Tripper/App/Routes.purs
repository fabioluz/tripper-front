module Tripper.App.Routes where

import Tripper.Prelude hiding ((/), sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', path, root, segment, string)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Login
  | Register
  | Home
  | Users
  | User String

derive instance genericRoute :: Generic Route _
derive instance eqRoute      :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Register": "regiser" / noArgs
  , "Login": "login" / noArgs
  , "Home": noArgs
  , "Users": "users" / noArgs
  , "User": "user"  / string segment
  }
