module Elm.Debug
  ( log
  ) where

import Effect.Unsafe (unsafePerformEffect)
import Elm.Basics ((|>))
import Elm.Logger as Logger
import Prelude (class Show)

log :: forall a. Show a => String -> a -> a
log tag val = unsafePerformEffect (Logger.debug tag val) |> (\_ -> val)
