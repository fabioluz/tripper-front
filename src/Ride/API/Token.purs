module Tripper.API.Token where

import Tripper.Prelude
import Tripper.Shared.Utils.LocalStorage as LS
  
newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

tokenKey :: String
tokenKey = "token"

readToken :: ∀ m. MonadEffect m => m (Maybe Token)
readToken = liftEffect $ map Token <$> LS.read tokenKey

writeToken :: ∀ m. MonadEffect m => Token -> m Unit
writeToken token = liftEffect $ LS.write tokenKey $ unwrap token

removeToken :: ∀ m. MonadEffect m => m Unit
removeToken = liftEffect $ LS.remove tokenKey

