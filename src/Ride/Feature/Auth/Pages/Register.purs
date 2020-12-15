module Tripper.Feature.Auth.Register (State, Action (..), component) where
  
import Tripper.Prelude
import Tripper.Feature.Auth.Capability
import Tripper.Feature.Navigation.Capability
import Tripper.Feature.Translation.Capability

import Tripper.Shared.Utils.AsMaybe (asMaybe)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Data.Lens (Lens, lens, (%=))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tripper.App.Routes as Routes
import Tripper.Shared.Components.Alert as AL
import Tripper.Shared.Components.Button as B
import Tripper.Shared.Components.PageHeader as PH
import Tripper.Shared.Components.TextInput as TI
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Styles as Stl
import Tripper.Shared.Utils.KeyboardKeys as Keys
import Tripper.Shared.Validators.String as VS

type State =
  { form     :: RegisterFields
  , isValid  :: Boolean
  , errorMsg :: Maybe String
  , loading  :: Boolean
  }

type ChildSlots =
  ( pageHeader :: PH.Slot Int
  , text       :: TI.Slot Int
  , button     :: B.Slot Int
  , i18n       :: TR.Slot
  )

data Action
  = HandleForm
  | HandleKeyDown KeyboardEvent
  | HandleClientName TI.Output
  | HandleAdminEmail TI.Output
  | HandleAdminPassword TI.Output
  | HandleAdminCPassword TI.Output
  | HandleAdminName TI.Output
  | HandleAdminNickName TI.Output

_pgh = SProxy :: SProxy "pageHeader"
_txt = SProxy :: SProxy "text"
_btn = SProxy :: SProxy "button"

component :: ∀ q i o m . ManageAuth m => I18N m => Navigate m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ i . i -> State
initialState _ =
  { form:
      { clientName: ""
      , adminEmail: ""
      , adminPassword: ""
      , adminCPassword: ""
      , adminName: ""
      , adminNickName: Nothing
      }
  , isValid: false
  , errorMsg: Nothing
  , loading: false
  }

render :: ∀ m . MonadAff m => I18N m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.slot _pgh 0 PH.component "register_title" absurd
    , case state.errorMsg of
        Nothing -> HH.text ""
        Just e  -> AL.alertError $ TR.slot e
    , HH.div
        [ HP.classes [ Stl.row ] ]
        [ HH.div
            [ HP.classes [ Stl.form, Stl.colMd 6 ], HE.onKeyDown $ Just <<< HandleKeyDown ]
            [ HH.slot _txt 0 TI.component clientNameInput     $ Just <<< HandleClientName
            , HH.slot _txt 1 TI.component adminEmailInput     $ Just <<< HandleAdminEmail
            , HH.slot _txt 2 TI.component adminPasswordInput  $ Just <<< HandleAdminPassword
            , HH.slot _txt 3 TI.component adminCPasswordInput $ Just <<< HandleAdminCPassword
            , HH.slot _txt 4 TI.component adminNameInput      $ Just <<< HandleAdminName
            , HH.slot _txt 5 TI.component adminNicknameInput  $ Just <<< HandleAdminNickName
            , HH.div
                [ HP.classes [ Stl.formGroup, Stl.flex, Stl.flexRowRev, Stl.pt 3 ] ]
                [ HH.slot _btn 0 B.component registerInput $ Just <<< const HandleForm ]
            ]     
        ]
    ]
  where
    clientNameInput :: TI.Input  
    clientNameInput =
      { id: "client-name-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_clientName"
      , value: ""
      , features: []
      , validators: [ VS.Required $ wrap "register_error_clientNameRequired" ]
      }

    adminEmailInput :: TI.Input
    adminEmailInput =
      { id: "admin-email-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_adminEmail"
      , value: ""
      , features: []
      , validators: [ VS.Required $ wrap "register_error_adminEmailRequired" ]
      }

    adminPasswordInput :: TI.Input
    adminPasswordInput =
      { id: "admin-password-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_adminPassword"
      , value: ""
      , features: [ TI.Private ]
      , validators: [ VS.Required $ wrap "register_error_adminPasswordRequired" ]
      }

    adminCPasswordInput :: TI.Input
    adminCPasswordInput =
      { id: "admin-confirm-password-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_adminConfirmPassword"
      , value: ""
      , features: [ TI.Private ]
      , validators:
          [ VS.Required $ wrap "register_error_adminConfirmPasswordRequired"
          , VS.EqualsTo state.form.adminPassword $ wrap "register_error_adminConfirmPasswordMustMatch"
          ]
      }

    adminNameInput :: TI.Input
    adminNameInput =
      { id: "admin-name-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_adminName"
      , value: ""
      , features: []
      , validators: [ VS.Required $ wrap "register_error_adminNameRequired" ]
      }

    adminNicknameInput :: TI.Input
    adminNicknameInput =
      { id: "admin-nickname-input"
      , classes: [ Stl.formGroup ]
      , label: Just "register_adminNickname"
      , value: ""
      , features: []
      , validators: []
      }

    registerInput :: B.Input
    registerInput = 
      { id: "register-button"
      , classes: [ Stl.mt 3 ]
      , label: "register_register"
      , loading: state.loading
      , features: [ B.Primary, B.Large ]
      }   

handleAction :: ∀ o m . ManageAuth m => Navigate m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleKeyDown e ->
    when (key e == Keys.enter) do
      handleAction HandleForm

  HandleClientName o ->
    case o of
      TI.Changed v -> formL %= _ { clientName = v }
      _            -> pure unit

  HandleAdminEmail o ->
    case o of
      TI.Changed v -> formL %= _ { adminEmail = v }
      _            -> pure unit

  HandleAdminPassword o ->
    case o of
      TI.Changed v -> formL %= _ { adminPassword = v }
      _            -> pure unit

  HandleAdminCPassword o ->
    case o of
      TI.Changed v -> formL %= _ { adminCPassword = v }
      _            -> pure unit
  
  HandleAdminName o ->
    case o of
      TI.Changed v -> formL %= _ { adminName = v }
      _            -> pure unit

  HandleAdminNickName o ->
    case o of
      TI.Changed v -> formL %= _ { adminNickName = asMaybe v }
      _            -> pure unit

  HandleForm -> do
    validateForm >>= traverse_ \fields -> do
      H.modify_ _ { errorMsg = Nothing, loading = true }
      register <- registerClient fields

      case register of
        Left err -> do
          let
            errorMsg :: String
            errorMsg = registerErrorMsg err
            
          H.modify_ _ { errorMsg = Just errorMsg, loading = false }

        Right _ -> do
          navigate Routes.Login
      
formL :: Lens State State RegisterFields RegisterFields
formL = lens _.form $ _ { form = _ }

validateForm :: ∀ o m . H.HalogenM State Action ChildSlots o m (Maybe RegisterFields)
validateForm = do
  txts <- H.queryAll _txt $ H.request TI.IsValid
  whenAllM isTrue txts do
    H.gets _.form
    
loginFields :: State -> LoginFields
loginFields state =
  { email: state.form.adminEmail
  , password: state.form.adminPassword
  }

registerErrorMsg :: Error -> String
registerErrorMsg = case _ of
  NetworkError      -> "register_error_network"
  ValidationError _ -> "register_error_validation"
