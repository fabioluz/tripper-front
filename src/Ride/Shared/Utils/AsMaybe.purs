module Tripper.Shared.Utils.AsMaybe where
  
import Tripper.Prelude
import Data.String as String

class AsMaybe a where
  asMaybe :: a -> Maybe a

instance asMaybeString :: AsMaybe String where
  asMaybe x 
    | String.null x = Nothing
    | otherwise     = Just x