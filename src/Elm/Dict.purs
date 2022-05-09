module Elm.Dict
  ( Dict
  , empty
  , init
  , initWith
  , keys
  , singleton
  , toArray
  , values
  ) where

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Elm.Array as Array
import Elm.Basics ((>>))
import Prelude (class Ord, class Functor)
import Prelude as Prelude

newtype Dict k v
  = Dict (Map k v)

empty :: forall k v. Dict k v
empty = Dict Map.empty

singleton :: forall k v. Ord k => k -> v -> Dict k v
singleton k v = init [ Tuple k v ]

init :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Dict k v
init = Map.fromFoldable >> Dict

initWith :: forall f v k. Functor f => Ord k => Foldable f => (v -> k) -> f v -> Dict k v
initWith toKey = Prelude.map (\v -> Tuple (toKey v) v) >> init

toArray :: forall k v. Dict k v -> Array (Tuple k v)
toArray (Dict m) = Map.toUnfoldable m

keys :: forall k v. Dict k v -> Array k
keys = toArray >> Array.map Tuple.fst

values :: forall k v. Dict k v -> Array v
values = toArray >> Array.map Tuple.snd
