module Elm.Node.Logger
  ( debug
  , error
  , info
  , warn
  ) where

import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Elm.Basics ((++))
import Prelude (class Show, Unit, show)

debug :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
debug tag val = Console.log ("DEBUG - " ++ tag ++ ": " ++ show val)

info :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
info tag val = Console.info ("INFO - " ++ tag ++ ": " ++ show val)

warn :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
warn tag val = Console.warn ("WARN - " ++ tag ++ ": " ++ show val)

error :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
error tag val = Console.error ("ERROR - " ++ tag ++ ": " ++ show val)
