module Tripper.Feature.Translation.Effects where
  
import Tripper.Prelude
import Tripper.API.Endpoint
import Tripper.API.Request
import Tripper.Feature.Translation.Capability

import Effect.Ref as Ref
import Tripper.Shared.FFI.I18N as Translator

setLanguage :: ∀ env m . MonadAPI env m => HasI18N env => String -> m Unit
setLanguage lang = do
  let 
    params :: RequestParams
    params =
      { endpoint: Translation lang
      , method: GET
      , body: Nothing
      , headers: []
      }

  response      <- mkRequest params
  translatorRef <- asks translatorRef
  for_ response \res -> do
    let
      translator :: String -> String
      translator = Translator.create res.body
      
    liftEffect $ Ref.write translator translatorRef 

getTranslator :: ∀ env m . MonadEffect m => MonadAsk env m => HasI18N env => m Translator
getTranslator = do
  translatorRef <- asks translatorRef
  liftEffect $ Ref.read translatorRef

translate :: ∀ env m . MonadEffect m => MonadAsk env m => HasI18N env => String -> m String
translate key = do
  translator <- getTranslator
  pure $ translator key
