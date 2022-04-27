module Elm.Effect
  ( andMap
  , andThen
  , map
  ) where

import Effect (Effect)
import Elm.Internal.Shortcut as Shortcut

map :: forall a b. (a -> b) -> Effect a -> Effect b
map = Shortcut.map

andMap :: forall a b. Effect a -> Effect (a -> b) -> Effect b
andMap = Shortcut.andMap

andThen :: forall a b. (a -> Effect b) -> Effect a -> Effect b
andThen = Shortcut.andThen
