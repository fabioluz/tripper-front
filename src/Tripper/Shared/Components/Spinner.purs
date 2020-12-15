module Tripper.Shared.Components.Spinner where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA

spinner :: ∀ w i . HH.HTML w i
spinner = 
  HH.span
    [ HP.class_ baseClass, HA.hidden "true", HA.role "status" ]
    []
  where
    baseClass :: HH.ClassName
    baseClass = HH.ClassName "spinner-border"