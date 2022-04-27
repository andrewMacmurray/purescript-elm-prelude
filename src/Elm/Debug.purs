module Elm.Debug
  ( log
  ) where

import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Elm.Basics ((++), (|>))
import Prelude (class Show, show)

log :: forall a. Show a => String -> a -> a
log tag val = unsafePerformEffect (Console.log (tag ++ ": " ++ show val)) |> (\_ -> val)
