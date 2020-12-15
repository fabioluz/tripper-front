module Tripper.App.Main (Action (..), Query (..), component) where

import Tripper.Prelude
import Tripper.AppM
import Tripper.App.Routes
import Tripper.Feature.Auth.Capability
import Tripper.Feature.Translation.Capability
import Tripper.Shared.Utils.Types

import Control.Monad.Rec.Class (forever)
import Effect.Aff.Bus (BusRW)
import Effect.Console (logShow)

import Effect.Aff.Bus as Bus
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Tripper.Shared.Styles as Stl
import Tripper.App.Components.TopNavBar as TopNavBar
import Tripper.Feature.Auth.Login as Login
import Tripper.Feature.Auth.Register as Register
import Tripper.Feature.Home as Home
import Tripper.Feature.User.List as Users

type State =
  { loading  :: Boolean
  , curUser  :: Maybe CurrentUser
  , curRoute :: Maybe Route
  }

type ChildSlots =
  ( part :: OpaqueSlot Int )

data Action 
  = Initialize
  | HandleCurrentUser (Maybe CurrentUser)

data Query a
  = Navigate Route a

_part = SProxy :: SProxy "part"

component :: ∀ i o . H.Component HH.HTML Query i o AppM
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
    }

initialState :: ∀ i . i -> State
initialState _ =
  { loading: true
  , curUser: Nothing
  , curRoute: Just Login
  }

render :: State -> H.ComponentHTML Action ChildSlots AppM
render { loading, curUser, curRoute } =
  HH.div_
    [ case loading of
        true ->
          HH.text "Loading..."
        false ->
          HH.div_
            [ HH.slot _part 0 TopNavBar.component { curUser } absurd 
            , HH.div
                [ HP.class_ Stl.pageContainer ]
                [ case curRoute of
                    Nothing    -> HH.slot _part 0 Login.component unit absurd
                    Just route -> handleRoute route
                ]
            ]
    ]

handleAction :: ∀ o . Action -> H.HalogenM State Action ChildSlots o AppM Unit
handleAction = case _ of
  Initialize -> do
    userBus <- getUserBus
    _       <- H.subscribe $ userEventSource userBus
    setLanguage "en"
    identifyUser
    H.modify_ _ { loading = false }

  HandleCurrentUser curUser -> do
    H.modify_ _ { curUser = curUser }

handleQuery :: ∀ a o . Query a -> H.HalogenM State Action ChildSlots o AppM (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    { curUser, curRoute } <- H.get

    let
      publicPages :: Array Route
      publicPages = [Login, Register]

      isAllowed :: Boolean
      isAllowed = case curUser of
        Nothing -> route `elem` publicPages
        Just _  -> route `notElem` publicPages

      newRoute :: Maybe Route
      newRoute = if isAllowed then Just route else Just Login

    liftEffect $ logShow route
    when (curRoute /= newRoute) do
      H.modify_ _ { curRoute = newRoute }

    pure (Just a)

handleRoute :: Route -> H.ComponentHTML Action ChildSlots AppM
handleRoute = case _ of
  Login    -> HH.slot _part 1 Login.component    unit absurd
  Register -> HH.slot _part 2 Register.component unit absurd
  Home     -> HH.slot _part 3 Home.component     unit absurd
  Users    -> HH.slot _part 4 Users.component    unit absurd
  User s   -> HH.slot _part 5 Users.component    unit absurd

userEventSource :: ∀ m . MonadAff m => BusRW (Maybe CurrentUser) -> ES.EventSource m Action
userEventSource curUserBus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever do
      curUser <- Bus.read curUserBus
      ES.emit emitter $ HandleCurrentUser curUser

    pure $ ES.Finalizer do
      killFiber (error "Event source closed") fiber

-- routePage :: Maybe Route -> H.ComponentHTML Action ChildSlots AppM
-- routePage = case _ of
--   Nothing    -> HH.slot _part 0 Login.component unit absurd
--   Just route ->
--     case route of
--       Login    -> HH.slot _part 1 Login.component unit absurd
--       Register -> HH.slot _part 2 Login.component unit absurd
--       Home     -> HH.slot _part 3 Login.component unit absurd
--       Users    -> HH.slot _part 4 Users.component unit absurd
--       User s   -> HH.slot _part 5 Login.component unit absurd