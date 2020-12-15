module Tripper.Shared.Components.PageHeader
( State
, Input
, Slot
, component
) where

import Tripper.Prelude
import Tripper.Feature.Translation.Capability

import Tripper.Shared.Utils.Types (OpaqueSlot)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Styles as Stl

type State = String

type Input = String

type Slot slot = OpaqueSlot slot

type ChildSlots = ( i18n :: TR.Slot )

component :: ∀ qry out m . I18N m => H.Component HH.HTML qry Input out m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: ∀ act m . I18N m => State -> H.ComponentHTML act ChildSlots m
render state =
  HH.div
    [ HP.classes [ Stl.display 4, Stl.mb 5  ] ]
    [ TR.slot state ]