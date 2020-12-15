module Tripper.Prelude
( module Prelude
, module Data.Maybe
, module Data.Either
, module Data.Foldable
, module Data.Traversable
, module Data.Newtype
, module Data.Symbol
, module Effect
, module Effect.Class
, module Effect.Aff
, module Effect.Aff.Class
, module Control.Monad.Reader
, Error (..)
, mkGenResError
, isTrue
, whenAllM
) where

import Prelude
import Control.Monad.Reader
import Data.Maybe 
import Data.Either
import Data.Foldable hiding (length)
import Data.Traversable
import Data.Newtype hiding (traverse)
import Data.Symbol
import Effect
import Effect.Class
import Effect.Aff hiding (Error)
import Effect.Aff.Class

import Data.Argonaut (Json)
import Data.Argonaut.Core (jsonSingletonObject, fromString)

data Error
  = NetworkError
  | ValidationError { errors :: Json }

mkGenResError :: { errors :: Json }
mkGenResError = { errors }
  where
    errors :: Json
    errors = jsonSingletonObject "unknown" (fromString "unknown")

isTrue :: Boolean -> Boolean
isTrue x = x == true

whenAllM :: forall t11 t13 t4 t6. Foldable t4 => Functor t11 => Applicative t11 => (t6 -> Boolean) -> t4 t6 -> t11 t13 -> t11 (Maybe t13)
whenAllM pred f x =
  if all pred f
    then x <#> Just
    else pure Nothing
