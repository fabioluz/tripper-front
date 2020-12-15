module Tripper.Feature.User.List (State, Action (..), component) where

import Tripper.Prelude
import Tripper.Feature.Translation.Capability
import Tripper.Feature.User.Capability 

import Tripper.Feature.User.Types (User)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tripper.Shared.Components.Translate as TR
import Tripper.Shared.Components.Spinner as SP
import Tripper.Shared.Styles as Stl

type State =
  { users   :: Array User
  , loading :: Boolean
  }

type ChildSlots =
  ( i18n :: TR.Slot )  

data Action = Initialize

component :: ∀ i o q m . I18N m => ManageUser m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  
initialState :: ∀ i . i -> State
initialState _ =
  { users: []
  , loading: true
  }

render :: ∀ m . I18N m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.table
        [ HP.class_ Stl.tableSmall ]
        [ HH.thead
            [ HP.class_ Stl.theadLight ]
            [ HH.tr_ 
                [ HH.th_ [ TR.slot "Name" ]
                , HH.th_ [ TR.slot "Email" ]
                , HH.th_ [ TR.slot "Nickname" ]
                ]
            ]
        , HH.tbody_ case state.loading of
            true  -> [ HH.tr_ [ HH.td_ [ SP.spinner ] ] ]
            false -> state.users <#> \user ->
              HH.tr_
                [ HH.td_ [ HH.text user.name ]
                , HH.td_ [ HH.text user.email ]
                , HH.td_ [ HH.text $ fromMaybe "" user.nickName ]
                ]
        ]
    ]

handleAction :: ∀ o m . ManageUser m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    mayUsers <- getUsers
    for_ mayUsers \users -> do
      H.modify_ _ { users = users, loading = false }