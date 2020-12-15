module Tripper.Shared.Components.Button
( State
, Input
, Slot
, Feature (..)
, Action (..)
, Output (..)
, Query (..)
, component
) where
  
import Tripper.Prelude
import Tripper.Feature.Translation.Capability

import Data.Array ((:))
import Tripper.Shared.Components.Spinner (spinner)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tripper.Shared.Components.Translate as TR

type State = 
  { id       :: String
  , classes  :: Array H.ClassName
  , label    :: String
  , loading  :: Boolean
  , features :: Array Feature
  }
  
type Input =
  { id       :: String
  , classes  :: Array H.ClassName
  , label    :: String
  , features :: Array Feature
  , loading  :: Boolean
  }

type Slot = H.Slot Query Output

type ChildSlots = ( i18n :: TR.Slot )

data Feature
  = Primary
  | Secondary
  | Success
  | Danger
  | Link
  | Small
  | Large
  | Block

data Action = Receive State | Click

data Output = Clicked

data Query a = SetLoading Boolean a

component :: ∀ m . I18N m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
    }

initialState :: Input -> State
initialState input =
  { id: input.id
  , classes: input.classes
  , label: input.label
  , features: input.features
  , loading: false
  }

render :: ∀ m . I18N m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HP.id_ state.id 
    , HP.disabled state.loading
    , HP.classes buttonClasses
    , HE.onClick $ Just <<< const Click
    ]
    [ HH.div
        [ HP.classes containerClasses ]
        [ case state.loading of
            false -> HH.text ""
            true  -> HH.div [ HP.class_ loaderClass ] [ spinner ]
        , HH.span
            [ HP.class_ labelClass ]
            [ TR.slot state.label ]
        ]
    ]
  where
    baseClass :: H.ClassName
    baseClass = wrap "btn"

    containerClasses :: Array H.ClassName
    containerClasses
      | state.loading = [ wrap "btn-container", wrap "loading" ]
      | otherwise     = [ wrap "btn-container" ]

    loaderClass :: H.ClassName
    loaderClass = wrap "btn-loader"

    labelClass :: H.ClassName
    labelClass = wrap "btn-label"

    featureClasses :: Array H.ClassName
    featureClasses = state.features <#> wrap <<< case _ of 
      Primary   -> "btn-primary"
      Secondary -> "btn-secondary"
      Success   -> "btn-success"
      Danger    -> "btn-danger"
      Link      -> "btn-link"
      Small     -> "btn-sm"
      Large     -> "btn-lg"
      Block     -> "btn-block"

    buttonClasses :: Array H.ClassName
    buttonClasses = baseClass : featureClasses <> state.classes

handleAction :: ∀ m . Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Receive state -> H.put state
  Click         -> H.raise Clicked

handleQuery :: ∀ a m . Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
handleQuery = case _ of
  SetLoading loading next -> do
    H.modify_ _ { loading = loading }
    pure $ Just next