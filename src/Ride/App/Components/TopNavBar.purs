module Tripper.App.Components.TopNavBar
( Input
, State
, MenuItem
, Action (..)
, component
) where
  
import Tripper.Prelude
import Tripper.Feature.Auth.Capability
import Tripper.Feature.Translation.Capability
import Tripper.Feature.Navigation.Capability

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tripper.App.Routes as Routes
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Styles as Stl

type MenuItem =
  { label  :: String
  , action :: Maybe Action
  }

type Input = 
  { curUser :: Maybe CurrentUser }

type State = 
  { menuItemsLeft  :: Array MenuItem
  , menuItemsRight :: Array MenuItem
  }

type ChildSlots =
  ( i18n :: TR.Slot )

data Action
  = Receive Input
  | Logout
  | HandleRoute Routes.Route

component :: ∀ q o m . I18N m => ManageAuth m => Navigate m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  
initialState :: Input -> State
initialState { curUser } =
  case curUser of
    Nothing ->
      { menuItemsLeft:
          [ { label: "menu_register", action: Just $ HandleRoute Routes.Register }
          ]
      , menuItemsRight:
          [ { label: "menu_login", action: Just $ HandleRoute Routes.Login }
          ]
      }
    Just _ ->
      { menuItemsLeft:
          [ { label: "menu_home", action: Just $ HandleRoute Routes.Home }
          , { label: "menu_users", action: Just $ HandleRoute Routes.Users }
          ]
      , menuItemsRight:
          [ { label: "menu_logout", action: Just Logout } ]
      }

render :: ∀ m . I18N m => State -> H.ComponentHTML Action ChildSlots m
render state = 
  HH.nav
    [ HP.class_ Stl.mainNavBar ]
    [ HH.div
        [ HP.class_ Stl.container ]
        [ HH.div
            [ HP.classes [ Stl.collapse, Stl.navBarCollapse ] ]
            [ HH.a
                [ HP.class_ Stl.navBarBrand, HP.href "#" ]
                [ HH.text "Ride" ]
            , HH.ul
                [ HP.classes [ Stl.navBarNav, Stl.mrAuto ] ]
                ( menuItem <$> state.menuItemsLeft )
            , HH.ul
                [ HP.class_ Stl.navBarNav ]
                ( menuItem <$> state.menuItemsRight )
            ]
        ] 
    ]

handleAction :: ∀ o m . ManageAuth m => Navigate m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Receive i     -> H.put $ initialState i
  Logout        -> logoutUser *> navigate Routes.Login
  HandleRoute r -> navigate r

menuItem :: ∀ m . I18N m => MenuItem -> H.ComponentHTML Action ChildSlots m
menuItem i =
  HH.li
    [ HP.class_ Stl.navItem ]
    [ HH.a
        [ HP.class_ Stl.navLink
        , HP.href "javascript:void(0);"
        , HE.onClick \_ -> i.action
        ]
        [ TR.slot i.label ] 
    ]