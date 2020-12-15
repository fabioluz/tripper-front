module Tripper.Shared.Components.Alert where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Type = Success | Danger | Info

alert :: ∀ w i . Type -> HH.HTML w i -> HH.HTML w i
alert _type content =
  HH.div
    [ HP.classes [ baseClass, typeClass ] ]
    [ content ]
  where
    baseClass :: HH.ClassName
    baseClass = HH.ClassName "alert"

    typeClass :: HH.ClassName
    typeClass = HH.ClassName case _type of
      Success -> "alert-success"
      Danger  -> "alert-danger"
      Info    -> "alert-info"

alertError :: ∀ w i . HH.HTML w i -> HH.HTML w i
alertError = alert Danger