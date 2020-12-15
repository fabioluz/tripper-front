module Tripper.Shared.Validators where
  
import Tripper.Prelude

newtype ErrorMsg = ErrorMsg String

derive instance newtypeErrorMsg :: Newtype ErrorMsg _
derive newtype instance eqErrorMsg :: Eq ErrorMsg