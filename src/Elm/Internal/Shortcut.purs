module Elm.Internal.Shortcut
  ( map
  , map2
  , map3
  , map4
  , map5
  , map6
  , map7
  , map8
  , map9
  , andMap
  , andThen
  , afterwards
  , blank
  ) where

import Prelude
import Prelude as Prelude

andThen :: forall m a b. Monad m => (a -> m b) -> m a -> m b
andThen b a = a >>= b

afterwards :: forall m a b. Monad m => m b -> m a -> m b
afterwards b a = a >>= (\_ -> b)

map :: forall m a value. Functor m => (a -> value) -> m a -> m value
map = Prelude.map

map2 :: forall m a b value. Applicative m => (a -> b -> value) -> m a -> m b -> m value
map2 func a b = pure func <*> a <*> b

map3 :: forall m a b c value. Applicative m => (a -> b -> c -> value) -> m a -> m b -> m c -> m value
map3 func a b c = pure func <*> a <*> b <*> c

map4 :: forall m a b c d value. Applicative m => (a -> b -> c -> d -> value) -> m a -> m b -> m c -> m d -> m value
map4 func a b c d = pure func <*> a <*> b <*> c <*> d

map5 :: forall m a b c d e value. Applicative m => (a -> b -> c -> d -> e -> value) -> m a -> m b -> m c -> m d -> m e -> m value
map5 func a b c d e = pure func <*> a <*> b <*> c <*> d <*> e

map6 :: forall m a b c d e f value. Applicative m => (a -> b -> c -> d -> e -> f -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m value
map6 func a b c d e f = pure func <*> a <*> b <*> c <*> d <*> e <*> f

map7 :: forall m a b c d e f g value. Applicative m => (a -> b -> c -> d -> e -> f -> g -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m value
map7 func a b c d e f g = pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g

map8 :: forall m a b c d e f g h value. Applicative m => (a -> b -> c -> d -> e -> f -> g -> h -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m value
map8 func a b c d e f g h = pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h

map9 :: forall m a b c d e f g h i value. Applicative m => (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m i -> m value
map9 func a b c d e f g h i = pure func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i

andMap :: forall m a b. Applicative m => m a -> m (a -> b) -> m b
andMap m mf = mf <*> m

blank :: forall m. Monad m => m Unit
blank = pure unit
