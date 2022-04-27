module Elm.Maybe
  ( andMap
  , andThen
  , map
  , map2
  , map3
  , map4
  , map5
  , module Data.Maybe
  , withDefault
  ) where

import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Elm.Internal.Shortcut as Shortcut

withDefault :: forall a. a -> Maybe a -> a
withDefault = Maybe.fromMaybe

map :: forall a b. (a -> b) -> Maybe a -> Maybe b
map = Shortcut.map

map2 :: forall a b value. (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 = Shortcut.map2

map3 :: forall a b c value. (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 = Shortcut.map3

map4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 = Shortcut.map4

map5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
map5 = Shortcut.map5

andMap :: forall a b. Maybe a -> Maybe (a -> b) -> Maybe b
andMap = Shortcut.andMap

andThen :: forall a b. (a -> Maybe b) -> Maybe a -> Maybe b
andThen = Shortcut.andThen
