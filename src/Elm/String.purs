module Elm.String
  ( concat
  , join
  , length
  , split
  , toUpper
  ) where

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.String (Pattern(..))
import Data.String as String

concat :: forall f. Foldable f => f String -> String
concat = join ""

join :: forall f. Foldable f => String -> f String -> String
join = Foldable.intercalate

split :: String -> String -> Array String
split pattern = String.split (Pattern pattern)

toUpper :: String -> String
toUpper = String.toUpper

length :: String -> Int
length = String.length
