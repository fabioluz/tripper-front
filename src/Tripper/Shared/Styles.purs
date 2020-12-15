module Tripper.Shared.Styles where

import Tripper.Prelude
import Halogen (ClassName (..))

loginContainer :: Array ClassName
loginContainer = map ClassName
  [ "d-flex"
  , "flex-column"
  , "justify-content-center"
  , "align-items-center"
  , "mx-auto"
  , "p-3"
  , "login-container"
  ]

form :: ClassName
form = ClassName "form"

formControl :: ClassName
formControl = ClassName "form-control"

invalidFormControl :: ClassName
invalidFormControl = ClassName "invalid"

invalidFeedback :: ClassName
invalidFeedback = ClassName "invalid-feedback"

formGroup :: ClassName
formGroup = ClassName "form-group"

container :: ClassName
container = ClassName "container"

pageContainer :: ClassName
pageContainer = ClassName "container page-container"

collapse :: ClassName
collapse = ClassName "collapse"

mrAuto :: ClassName
mrAuto = ClassName "mr-auto"

navBarCollapse :: ClassName
navBarCollapse = ClassName "navbar-collapse"

mainNavBar :: ClassName
mainNavBar = ClassName "navbar navbar-expand navbar-dark bg-dark"

navBarBrand :: ClassName
navBarBrand = ClassName "navbar-brand"

navBarNav :: ClassName
navBarNav = ClassName "navbar-nav"

navItem :: ClassName
navItem = ClassName "nav-item"

navLink :: ClassName
navLink = ClassName "nav-link"  

tableSmall :: ClassName
tableSmall = ClassName "table table-sm"

theadLight :: ClassName
theadLight = ClassName "thead-light"
  
row :: ClassName
row = ClassName "row"

jcMdCenter :: ClassName
jcMdCenter = ClassName "justify-content-md-center"

colMd :: Int -> ClassName
colMd n = ClassName $ "col-md-" <> show n

flex :: ClassName
flex = ClassName "d-flex"

flexRowRev :: ClassName
flexRowRev = ClassName "flex-row-reverse"

mt :: Int -> ClassName
mt n = ClassName $ "mt-" <> show n

mb :: Int -> ClassName
mb n = ClassName $ "mb-" <> show n

pt :: Int -> ClassName
pt n = ClassName $ "pt-" <> show n

display :: Int -> ClassName
display n = ClassName $ "display-" <> show n

spinnerBorder :: ClassName
spinnerBorder = ClassName "spinner-border"
