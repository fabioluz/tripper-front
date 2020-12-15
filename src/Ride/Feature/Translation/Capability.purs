module Tripper.Feature.Translation.Capability where
  
import Tripper.Prelude

import Effect.Ref (Ref)
import Halogen (HalogenM)

type Translator = String -> String

class HasI18N env where
  translatorRef :: env -> Ref Translator

class MonadAff m <= I18N m where
  setLanguage :: String -> m Unit
  translate   :: String -> m String

instance i18nHalogenM :: I18N m => I18N (HalogenM st act sl out m) where
  setLanguage = lift <<< setLanguage
  translate   = lift <<< translate