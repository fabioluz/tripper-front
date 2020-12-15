module Tripper.API.Endpoint where

import Tripper.Prelude hiding ((/), sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', print, root, segment)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/))
  
data Endpoint
  = Translation String
  | Login
  | Me
  | Clients
  | Users

derive instance genEndpoint :: Generic Endpoint _
derive instance eqRoute     :: Eq Endpoint

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Translation": "translation" / segment
  , "Login": "login" / noArgs
  , "Me": "me" / noArgs
  , "Clients": "clients" / noArgs
  , "Users": "users" / noArgs
  }

endpointUrl :: Endpoint -> String
endpointUrl = print endpointCodec