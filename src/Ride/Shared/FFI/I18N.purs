module Tripper.Shared.FFI.I18N where

import Data.Argonaut (Json)

foreign import create :: Json -> String -> String