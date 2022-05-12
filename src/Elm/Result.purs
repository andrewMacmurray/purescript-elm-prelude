module Elm.Result
  ( Result(..)
  , andThen
  , fromEither
  , fromMaybe
  , map
  , map2
  , map3
  , map4
  , map5
  , mapError
  , toEither
  , toMaybe
  , withDefault
  ) where

import Data.Either (Either)
import Data.Either as Either
import Elm.Basics
import Elm.Internal.Shortcut as Shortcut
import Elm.Maybe (Maybe(..))
import Prelude as Prelude

data Result error value
  = Ok value
  | Err error

instance monadResult :: Prelude.Monad (Result error)

instance bindResult :: Prelude.Bind (Result error) where
  bind result func = case result of
    Ok value -> func value
    Err error -> Err error

instance applicativeResult :: Prelude.Applicative (Result error) where
  pure = Ok

instance applyResult :: Prelude.Apply (Result error) where
  apply (Ok func) (Ok a) = Ok (func a)
  apply (Err error) _ = Err error
  apply (Ok _) (Err error) = Err error

instance functorResult :: Prelude.Functor (Result error) where
  map func result = case result of
    Ok value -> Ok (func value)
    Err error -> Err error

instance showResult :: (Prelude.Show err, Prelude.Show value) => Prelude.Show (Result err value) where
  show (Ok a) = "(Ok " ++ Prelude.show a ++ ")"
  show (Err e) = "(Err " ++ Prelude.show e ++ ")"

instance eqResult :: (Prelude.Eq x, Prelude.Eq a) => Prelude.Eq (Result x a) where
  eq (Ok a) (Ok a') = Prelude.eq a a'
  eq (Err x) (Err x') = Prelude.eq x x'
  eq _ _ = false

withDefault :: forall a b. a -> Result b a -> a
withDefault fallback result = case result of
  Ok value -> value
  Err _ -> fallback

map :: forall a x value. (a -> value) -> Result x a -> Result x value
map = Shortcut.map

map2 :: forall a b x value. (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 = Shortcut.map2

map3 :: forall a b c x value. (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
map3 = Shortcut.map3

map4 :: forall a b c d x value. (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
map4 = Shortcut.map4

map5 :: forall a b c d e x value. (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
map5 = Shortcut.map5

andThen :: forall a b c. (a -> Result c b) -> Result c a -> Result c b
andThen = Shortcut.andThen

mapError :: forall a b c. (a -> b) -> Result a c -> Result b c
mapError func result = case result of
  Ok value -> Ok value
  Err error -> Err (func error)

toMaybe :: forall a b. Result a b -> Maybe b
toMaybe result = case result of
  Ok value -> Just value
  Err _ -> Nothing

fromMaybe :: forall a b. a -> Maybe b -> Result a b
fromMaybe error maybe = case maybe of
  Just something -> Ok something
  Nothing -> Err error

fromEither :: forall a b. Either a b -> Result a b
fromEither either = case either of
  Either.Right a -> Ok a
  Either.Left b -> Err b

toEither :: forall a b. Result a b -> Either a b
toEither = case _ of
  Ok a -> Either.Right a
  Err b -> Either.Left b
