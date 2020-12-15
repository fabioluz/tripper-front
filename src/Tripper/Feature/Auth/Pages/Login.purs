module Tripper.Feature.Auth.Login (State, Action (..), component) where
  
import Tripper.Prelude
import Tripper.Feature.Auth.Capability 
import Tripper.Feature.Navigation.Capability 
import Tripper.Feature.Translation.Capability

import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tripper.App.Routes as Routes
import Tripper.Shared.Components.Alert as AL
import Tripper.Shared.Components.Button as B
import Tripper.Shared.Components.TextInput as TI
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Styles as Stl
import Tripper.Shared.Utils.KeyboardKeys as Keys
import Tripper.Shared.Validators.String as VS

type State =
  { email    :: String
  , password :: String
  , errorMsg :: Maybe String
  , loading  :: Boolean
  }

type ChildSlots =
  ( i18n   :: TR.Slot
  , text   :: TI.Slot Int
  , button :: B.Slot Int
  )
 
data Action 
  = HandleLogin
  | HandleKeyDown KeyboardEvent
  | HandleEmail TI.Output
  | HandlePassword TI.Output

_txt = SProxy :: SProxy "text"
_btn = SProxy :: SProxy "button"

component :: ∀ q i o m . ManageAuth m => Navigate m => I18N m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  
initialState :: ∀ i . i -> State
initialState _ =
  { email: ""
  , password: "" 
  , errorMsg: Nothing
  , loading: false
  }

render :: ∀ m . MonadAff m => I18N m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ HP.classes Stl.loginContainer ]
    [ HH.div 
      [ HP.class_ Stl.form, HE.onKeyDown $ Just <<< HandleKeyDown ]
      [ case state.errorMsg of
          Nothing -> HH.text ""
          Just x  -> AL.alertError $ TR.slot x
      , HH.slot _txt 0 TI.component emailInput    $ Just <<< HandleEmail       -- | email component
      , HH.slot _txt 1 TI.component passwordInput $ Just <<< HandlePassword    -- | password component
      , HH.slot _btn 0 B.component  loginInput    $ Just <<< const HandleLogin -- | button component
      ]
    ]  
  where
    emailInput :: TI.Input
    emailInput =
      { id: "email-input"
      , classes: [ Stl.formGroup ]
      , label: Just "login_email"
      , value: ""
      , validators: [ VS.Required $ wrap "login_error_emailRequired" ]
      , features: []
      }

    passwordInput :: TI.Input
    passwordInput =
      { id: "password-input"
      , classes: [ Stl.formGroup ]
      , label: Just "login_password"
      , value: ""
      , validators: [ VS.Required $ wrap "login_error_passwordRequired" ]
      , features: [ TI.Private ]
      }

    loginInput :: B.Input
    loginInput = 
      { id: "login-button"
      , classes: [ Stl.mt 3 ]
      , label: "login_button"
      , loading: state.loading
      , features: [ B.Primary, B.Large ]
      }    

handleAction
  :: ∀ o m
  .  ManageAuth m
  => Navigate m
  => I18N m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleKeyDown e ->
    when (key e == Keys.enter) do
      handleAction HandleLogin

  HandleEmail o ->
    case o of
      TI.Changed v -> H.modify_ _ { email = v }
      _            -> pure unit
  
  HandlePassword o ->
    case o of
      TI.Changed v -> H.modify_ _ { password = v }
      _            -> pure unit

  HandleLogin ->
    validateForm >>= traverse_ \fields -> do
      H.modify_ _ { errorMsg = Nothing, loading = true }
      result <- loginUser fields

      case result of
        Left err ->
          H.modify_ _ { errorMsg = Just $ loginErrorMsg err, loading  = false }

        Right _ ->
          navigate Routes.Home

validateForm :: ∀ o m . H.HalogenM State Action ChildSlots o m (Maybe LoginFields)
validateForm = do
  txts <- H.queryAll _txt $ H.request TI.IsValid
  whenAllM isTrue txts do
    H.gets \s -> { email: s.email, password: s.password }

loginErrorMsg :: Error -> String
loginErrorMsg = case _ of
  NetworkError      -> "login_error_network"
  ValidationError _ -> "login_error_validation"

   
   
