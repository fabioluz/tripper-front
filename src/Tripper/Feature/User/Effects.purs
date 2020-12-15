module Tripper.Feature.User.Effects where

import Tripper.Prelude
import Tripper.API.Request 
import Tripper.API.Endpoint
import Tripper.Feature.Auth.Capability 
import Tripper.Feature.User.Types

getMe :: ∀ env m . MonadAPI env m => m (Maybe CurrentUser)
getMe = do
  let
    params :: RequestParams
    params =
      { endpoint: Me
      , method: GET
      , body: Nothing
      , headers: []
      }

  response <- mkRequest params
  pure case response of
    Left _  -> Nothing
    Right r -> Just r.body

getUsers :: ∀ env m . MonadAPI env m => m (Either Error (Array User))
getUsers = do
  let
    params :: RequestParams
    params = 
      { endpoint: Users
      , method: GET
      , body: Nothing
      , headers: []
      }

  response <- mkRequest params
  pure $ _.body <$> response