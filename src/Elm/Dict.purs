module Elm.Dict
  ( Dict
  , empty
  , initWith
  , values
  ) where

import Elm.Basics ((>>), (|>))
import Prelude (class Ord, class Functor)
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Elm.Array as Array
import Prelude as Prelude

newtype Dict k v
  = Dict (Map k v)

empty :: forall k v. Dict k v
empty = Dict Map.empty

initWith ::
  forall f v k.
  Functor f =>
  Ord k =>
  Foldable f =>
  (v -> k) -> f v -> Dict k v
initWith toKey =
  Prelude.map (\v -> Tuple (toKey v) v)
    >> Map.fromFoldable
    >> Dict

values :: forall k v. Dict k v -> Array v
values (Dict m) = Map.toUnfoldable m |> Array.map Tuple.snd
