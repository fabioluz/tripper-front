module Tripper.Config where

import Tripper.Prelude
import Tripper.API.Request
import Tripper.Feature.Auth.Capability
import Tripper.Feature.Navigation.Capability
import Tripper.Feature.Translation.Capability

import Effect.Ref (Ref)
import Effect.Aff.Bus (BusRW)
import Routing.PushState (PushStateInterface)
import Effect.Ref as Ref
import Effect.Aff.Bus as Bus
import Routing.PushState as PushState

data Config = Config
  { baseURL     :: BaseURL
  , nav         :: PushStateInterface
  , currentUser :: Ref (Maybe CurrentUser)
  , userBus     :: BusRW (Maybe CurrentUser)
  , translator  :: Ref (Translator)
  }

acquireConfig :: Effect Config
acquireConfig = do
  nav         <- PushState.makeInterface
  currentUser <- Ref.new Nothing
  userBus     <- Bus.make
  translator  <- Ref.new identity
  pure $ Config
    { baseURL: BaseURL "http://localhost:8090"
    , nav
    , currentUser
    , userBus
    , translator
    }

instance hasBaseUrlConfig :: HasBaseURL Config where
  baseURL (Config cfg) = cfg.baseURL

instance hasNav :: HasNav Config where
  nav (Config cfg) = cfg.nav

instance hasCurrentUserConfig :: HasCurrentUser Config where
  curUserRef (Config cfg) = cfg.currentUser
  curUserBus (Config cfg) = cfg.userBus
 
instance hasI18NConfig :: HasI18N Config where
  translatorRef (Config cfg) = cfg.translator