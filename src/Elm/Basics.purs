module Elm.Basics
  ( (++)
  , (<<)
  , (<|)
  , (>>)
  , (|>)
  , Never
  , always
  , backPipe
  , compose
  , concat
  , forwardCompose
  , forwardPipe
  , never
  ) where

import Data.Monoid as Moniod
import Data.Void as Data.Void
import Prelude (class Semigroup)
import Prelude as Prelude

infixr 0 backPipe as <|

backPipe :: forall a b. (a -> b) -> a -> b
backPipe f x = f x

infixl 0 forwardPipe as |>

forwardPipe :: forall a b. a -> (a -> b) -> b
forwardPipe x f = f x

infixr 5 concat as ++

concat :: forall a. Semigroup a => a -> a -> a
concat = Moniod.(<>)

infixl 9 compose as <<

compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f (x))

infixr 9 forwardCompose as >>

forwardCompose :: forall a b c. (a -> b) -> (b -> c) -> a -> c
forwardCompose f g x = g (f x)

always :: forall a b. a -> b -> a
always = Prelude.const

type Never
  = Data.Void.Void

never :: forall a. Never -> a
never = Data.Void.absurd
