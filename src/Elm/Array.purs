module Elm.Array
  ( (:)
  , concat
  , concatMap
  , filterMap
  , get
  , isEmpty
  , map
  , map2
  , map3
  , map4
  , map5
  , module Array
  , repeat
  ) where

import Data.Array (cons, drop, filter, find, foldM, foldl, foldr, head, length, replicate, singleton, take) as Array
import Data.Array as ArrayInternal
import Elm.Internal.Shortcut as Shortcut
import Elm.Maybe (Maybe(..))
import Prelude ((<>), (==))

isEmpty :: forall a. Array a -> Boolean
isEmpty xs = Array.length xs == 0

filterMap :: forall a b. (a -> Maybe b) -> Array a -> Array b
filterMap f xs = Array.foldr (maybeCons f) [] xs

maybeCons :: forall a b. (a -> Maybe b) -> a -> Array b -> Array b
maybeCons f mx xs = case f mx of
  Just x -> Array.cons x xs
  Nothing -> xs

get :: forall a. Int -> Array a -> Maybe a
get i xs = ArrayInternal.index xs i

infixr 5 Array.cons as :

concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap f xs = concat (map f xs)

concat :: forall a. Array (Array a) -> Array a
concat = Array.foldr (<>) []

repeat :: forall a. Int -> a -> Array a
repeat = Array.replicate

map :: forall a b. (a -> b) -> Array a -> Array b
map = Shortcut.map

map2 :: forall a b value. (a -> b -> value) -> Array a -> Array b -> Array value
map2 = Shortcut.map2

map3 :: forall a b c value. (a -> b -> c -> value) -> Array a -> Array b -> Array c -> Array value
map3 = Shortcut.map3

map4 :: forall a b c d value. (a -> b -> c -> d -> value) -> Array a -> Array b -> Array c -> Array d -> Array value
map4 = Shortcut.map4

map5 :: forall a b c d e value. (a -> b -> c -> d -> e -> value) -> Array a -> Array b -> Array c -> Array d -> Array e -> Array value
map5 = Shortcut.map5
