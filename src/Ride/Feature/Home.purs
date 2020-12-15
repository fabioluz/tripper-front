module Tripper.Feature.Home (component) where

import Tripper.Prelude

import Halogen as H
import Halogen.HTML as HH

component :: ∀ i qry out m . H.Component HH.HTML qry i out m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: ∀ st w i . st -> HH.HTML w i
render _ = 
  HH.div_ [ ]