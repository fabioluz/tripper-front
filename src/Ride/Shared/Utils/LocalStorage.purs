module Tripper.Shared.Utils.LocalStorage where
  
import Tripper.Prelude
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

read :: String -> Effect (Maybe String)
read key = window >>= localStorage >>= getItem key

write :: String -> String -> Effect Unit
write key value = window >>= localStorage >>= setItem key value

remove :: String -> Effect Unit
remove key = window >>= localStorage >>= removeItem key