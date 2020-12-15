module Tripper.Shared.Validators.String (Validator (..), validate) where

import Tripper.Prelude hiding (null)

import Data.Array (uncons)
import Data.String (null)
import Halogen.Query.HalogenM (SubscriptionId (..))
import Tripper.Shared.Validators (ErrorMsg)
  
data Validator
  = Required ErrorMsg 
  | EqualsTo String ErrorMsg

derive instance eqValidator :: Eq Validator

emptySubscription :: SubscriptionId
emptySubscription = SubscriptionId 0

-- validate :: Array Validator -> String -> Either ErrorMsg String
-- validate vs s = foldl (*>) (Right s) results
--   where
--     results :: Array (Either ErrorMsg String)
--     results = map (runValidation s) vs

validate :: Array Validator -> String -> Either ErrorMsg String
validate vs s = go (Right s) vs
  where
    go :: Either ErrorMsg String -> Array Validator -> Either ErrorMsg String
    go result arr = case result of
      Left _  -> result
      Right _ -> case uncons arr of
        Nothing -> result
        Just x  -> go (runValidation s x.head) x.tail

runValidation :: String -> Validator -> Either ErrorMsg String
runValidation x (Required error)  
  | null x    = Left error
  | otherwise = Right x
runValidation x (EqualsTo y error)
  | x /= y    = Left error      
  | otherwise = Right x



