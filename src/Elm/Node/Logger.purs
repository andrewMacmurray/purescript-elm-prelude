module Elm.Node.Logger
  ( debug
  ) where

import Effect (Effect)
import Effect.Class.Console as Console
import Elm.Basics ((++))
import Prelude (class Show, Unit, show)

debug :: forall a. Show a => String -> a -> Effect Unit
debug tag val = Console.log (tag ++ ": " ++ show val)
