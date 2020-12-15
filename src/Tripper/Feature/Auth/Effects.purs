module Tripper.Feature.Auth.Effects where

import Tripper.Prelude
import Tripper.API.Endpoint
import Tripper.API.Request
import Tripper.API.Token
import Tripper.Feature.Auth.Capability

import Data.Argonaut (encodeJson)
import Effect.Aff.Bus (BusRW)
import Tripper.Feature.User.Effects (getMe)

import Effect.Ref as Ref
import Effect.Aff.Bus as Bus

getUserBus :: ∀ env m . MonadEffect m => MonadAsk env m => HasCurrentUser env => m (BusRW (Maybe CurrentUser))
getUserBus = asks curUserBus

loginUser :: ∀ env m . MonadAPI env m => HasCurrentUser env => LoginFields -> m (Either Error LoginRes)
loginUser fields = do
  let 
    params :: RequestParams
    params =
      { endpoint: Login
      , method: POST
      , body: Just $ encodeJson fields
      , headers: []
      }

  response   <- mkRequest params
  curUserRef <- asks curUserRef
  curUserBus <- asks curUserBus
  for response \res -> do
    let
      token :: String
      token = res.body.token

      curUser :: Maybe CurrentUser
      curUser = Just res.body.user

    writeToken $ Token token
    liftEffect $ Ref.write curUser curUserRef
    liftAff    $ Bus.write curUser curUserBus

    pure res.body

logoutUser :: ∀ env m . MonadAff m => MonadAsk env m => HasCurrentUser env => m Unit
logoutUser = do
  curUserRef <- asks curUserRef
  curUserBus <- asks curUserBus
  removeToken
  liftEffect $ Ref.write Nothing curUserRef
  liftAff    $ Bus.write Nothing curUserBus

identifyUser :: ∀ env m . MonadAPI env m => HasCurrentUser env => m Unit
identifyUser = do
  token <- readToken
  for_ token \_ -> do
    curUserRef <- asks curUserRef
    curUserBus <- asks curUserBus
    curUser    <- getMe
    for_ curUser \_ -> do
      liftEffect $ Ref.write curUser curUserRef
      liftAff    $ Bus.write curUser curUserBus

registerClient :: ∀ env m . MonadAPI env m => RegisterFields -> m (Either Error Unit)
registerClient fields = do
  let
    params :: RequestParams
    params =
      { endpoint: Clients
      , method: POST
      , body: Just $ encodeJson fields
      , headers: []
      }

  response <- mkRequest_ params
  pure $ _.body <$> response
