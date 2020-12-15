module Tripper.Shared.Components.Translate (State, Slot, slot) where

import Tripper.Prelude
import Tripper.Feature.Translation.Capability

import Tripper.Shared.Utils.Types (NoQuery, NoOutput, OpaqueSlot)

import Halogen as H
import Halogen.HTML as HH

type State = String

type Slot = OpaqueSlot String

data Action = Initialize | Receive String

_i18n = SProxy :: SProxy "i18n"

slot
  :: ∀ slots actions m
   . I18N m
  => String
  -> HH.HTML ( H.ComponentSlot HH.HTML
               ( i18n :: H.Slot NoQuery NoOutput String
               | slots
               ) m actions
             ) actions
slot input =
  HH.slot _i18n input component input absurd

component :: ∀ q o m . I18N m => H.Component HH.HTML q String o m
component =
  H.mkComponent
    { initialState: identity
    , render: HH.text
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
    }

handleAction :: ∀ o m . I18N m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    key <- H.get
    handleAction $ Receive key
  Receive key -> do
    translation <- translate key
    H.put translation
