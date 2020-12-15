module Tripper.Shared.Components.TextInput
( State
, Input
, Slot
, Feature (..)
, Output (..)
, Action (..)
, Query (..)
, component
) where

import Tripper.Prelude
import Tripper.Feature.Translation.Capability

import Control.Alt ((<|>))
import Tripper.Shared.Validators (ErrorMsg)
import Web.UIEvent.FocusEvent (FocusEvent)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tripper.Shared.Styles as Stl
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Validators.String as VS

type State =
  { id          :: String
  , classes     :: Array H.ClassName
  , label       :: Maybe String
  , value       :: String
  , validators  :: Array VS.Validator
  , errorMsg    :: Maybe ErrorMsg
  , exErrorMsg  :: Maybe ErrorMsg
  , placeholder :: Maybe String
  , private     :: Boolean
  , touched     :: Boolean
  }

type Input =
  { id         :: String
  , classes    :: Array H.ClassName
  , label      :: Maybe String
  , value      :: String
  , validators :: Array VS.Validator
  , features   :: Array Feature
  }

type Slot = H.Slot Query Output

type ChildSlots = ( i18n :: TR.Slot )

data Feature
  = Private
  | Placeholder String
  
data Action 
  = Receive Input  
  | ChangeValue String
  | Blur FocusEvent
  | Validate

data Output 
  = Changed String
  | Blurred
  | Validated Boolean

data Query a
  = IsValid (Boolean -> a)
  | GetValue (String -> a)
  | SetError (Maybe ErrorMsg) a

component :: ∀ m . MonadAff m => I18N m => H.Component HH.HTML Query Input Output m
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

defaultState :: Input -> State
defaultState input =
  { id: input.id
  , classes: input.classes
  , label: input.label
  , value: input.value
  , validators: input.validators
  , errorMsg: Nothing
  , exErrorMsg: Nothing
  , placeholder: Nothing
  , private: false
  , touched: false
  }
  
initialState :: Input -> State
initialState input = 
  let
    state :: State
    state = defaultState input

    applyFeat :: State -> Feature -> State
    applyFeat s = case _ of
      Private       -> s { private = true }
      Placeholder p -> s { placeholder = Just p }
  in
    foldl applyFeat state input.features

render :: ∀ m . I18N m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ HP.classes state.classes ]
    [ case state.label of
        Nothing -> HH.text ""
        Just l  -> HH.label [ HP.for state.id ] [ TR.slot l ]
    , HH.input
        [ HP.id_ state.id
        , HP.type_ inputType
        , HP.classes inputClasses
        , HP.value state.value
        , HP.autocomplete false
        , HE.onValueInput $ Just <<< ChangeValue
        , HE.onBlur       $ Just <<< Blur
        ]
    , case errorMsg of
        Nothing -> HH.text ""
        Just e  -> HH.div
          [ HP.class_ Stl.invalidFeedback ]
          [ TR.slot $ unwrap e ]
    ]
  where
    errorMsg :: Maybe ErrorMsg
    errorMsg =
      state.errorMsg <|> state.exErrorMsg

    inputType :: HP.InputType
    inputType =
      if state.private
      then HP.InputPassword
      else HP.InputText

    inputClasses :: Array HH.ClassName
    inputClasses = 
      if isNothing state.errorMsg
      then [ Stl.formControl ]
      else [ Stl.formControl, Stl.invalidFormControl ]

handleAction :: ∀ m . I18N m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Receive i -> do
    validators <- H.gets _.validators
    touched    <- H.gets _.touched

    when (touched && i.validators /= validators) do
      H.modify_ _ { validators = i.validators }
      handleAction Validate

  ChangeValue v -> do
    H.modify_ $ _ { value = v, touched = true }
    H.raise   $ Changed v
    handleAction Validate

  Blur _ -> do
    _        <- H.raise Blurred
    errorMsg <- H.gets _.errorMsg

    when (isNothing errorMsg) do
      handleAction Validate

  Validate -> do
    validators <- H.gets _.validators
    value      <- H.gets _.value
    
    let
      result :: Either ErrorMsg String
      result = VS.validate validators value

      errorMsg :: Maybe ErrorMsg 
      errorMsg = case result of
        Left e  -> Just e
        Right _ -> Nothing

    H.modify_ _ { errorMsg = errorMsg }

handleQuery :: ∀ a m . I18N m => Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
handleQuery = case _ of
  IsValid reply -> do
    handleAction Validate
    errorMsg   <- H.gets _.errorMsg
    exErrorMsg <- H.gets _.exErrorMsg
    pure $ Just $ reply $ isNothing $ errorMsg <|> exErrorMsg

  GetValue reply -> do
    value <- H.gets _.value
    pure $ Just $ reply value

  SetError exErrorMsg next -> do
    H.modify_ _ { exErrorMsg = exErrorMsg }
    pure $ Just next
        
-- debouncedChange :: ∀ m . MonadAff m => String -> ES.EventSource m Action
-- debouncedChange value = ES.affEventSource \emitter -> do
--   fiber <- forkAff do
--     delay $ Milliseconds 500.0
--     ES.emit emitter ChangeValue

--   pure $ ES.Finalizer do
--     killFiber (error "Event source finalized") fiber