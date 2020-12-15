module Tripper.Feature.User.Types where

import Tripper.Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)

newtype ClientId = ClientId String

derive instance newtypeClientId :: Newtype ClientId _

derive newtype instance decodeJsonClientId :: DecodeJson ClientId

newtype UserId = UserId String

derive instance newtypeUserId :: Newtype UserId _

derive newtype instance decodeJsonUserId :: DecodeJson UserId

type UserRep r =
  { clientId :: ClientId
  , userId   :: UserId
  , email    :: String
  , name     :: String
  , nickName :: Maybe String
  | r
  }

type User = UserRep ()

