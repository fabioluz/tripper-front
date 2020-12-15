module Tripper.API.Request 
( RequestParams
, BaseURL (..)
, class HasBaseURL
, class MonadAPI
, baseURL
, mkRequest
, mkRequest_
, module Data.HTTP.Method
, module RH
) where
  
import Tripper.Prelude
import Tripper.API.Endpoint
import Tripper.API.Token

import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.HTTP.Method (Method (..))

import Affjax as AX
import Affjax.StatusCode as SC
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.RequestHeader (RequestHeader (..)) as RH

newtype BaseURL = BaseURL String

derive instance newtypeBaseURL :: Newtype BaseURL _

class HasBaseURL env where
  baseURL :: env -> BaseURL

class (MonadAff m, MonadAsk env m, HasBaseURL env) <= MonadAPI env m

instance monadAPI :: (MonadAff m, MonadAsk env m, HasBaseURL env) => MonadAPI env m

type RequestParams = 
  { endpoint :: Endpoint
  , method   :: Method
  , body     :: Maybe Json
  , headers  :: Array RH.RequestHeader
  }

defaultRequest
  :: ∀ a
  .  BaseURL
  -> Maybe Token
  -> Method
  -> String
  -> Array RH.RequestHeader
  -> RF.ResponseFormat a
  -> Maybe Json
  -> AX.Request a
defaultRequest baseUrl token method url headers format body = 
  { url: unwrap baseUrl <> url
  , method: Left method
  , headers: case token of
      Nothing -> headers
      Just t  -> RH.RequestHeader "Authorization" ("Bearer " <> unwrap t) : headers
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: format
  }

mkRequest :: ∀ a env m . MonadAPI env m => DecodeJson a => RequestParams -> m (Either Error (AX.Response a))
mkRequest { endpoint, method, body, headers } = do
  baseUrl <- asks baseURL
  token   <- readToken

  let
    url :: String
    url = endpointUrl endpoint

    requestParams :: AX.Request Json
    requestParams = defaultRequest baseUrl token method url headers RF.json body

  response <- liftAff $ AX.request requestParams
  pure case response of
    Left _    -> Left NetworkError
    Right res -> decodeBody res

mkRequest_ :: ∀ env m . MonadAPI env m => RequestParams -> m (Either Error (AX.Response Unit))
mkRequest_ { endpoint, method, body, headers } = do
  baseUrl <- asks baseURL
  token   <- readToken

  let
    url :: String
    url = endpointUrl endpoint

    requestParams :: AX.Request Unit
    requestParams = defaultRequest baseUrl token method url headers RF.ignore body

  response <- liftAff $ AX.request requestParams
  pure $ lmap (const NetworkError) response

decodeBody :: ∀ a . DecodeJson a => AX.Response Json -> Either Error (AX.Response a)
decodeBody res =
  case isSuccess res of
    true ->
      case decodeJson res.body of
        Left _  -> Left  $ NetworkError
        Right a -> Right $ res { body = a }
    false ->
      case decodeJson res.body of
        Left _  -> Left $ ValidationError mkGenResError
        Right a -> Left $ ValidationError a

statusCode :: ∀ a . AX.Response a -> Int
statusCode { status } = code
  where
    SC.StatusCode code = status

status401 :: ∀ a . AX.Response a -> Boolean
status401 res = statusCode res == 401 

status200 :: ∀ a . AX.Response a -> Boolean
status200 res = statusCode res == 200

status201 :: ∀ a . AX.Response a -> Boolean
status201 res = statusCode res == 201

isSuccess :: ∀ a . AX.Response a -> Boolean
isSuccess res = status >= 200 && status <= 299
  where
    status = statusCode res
