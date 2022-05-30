module Elm.Logger
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
debug tag val = Console.log (message Magenta tag val)

info :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
info tag val = Console.info (message Cyan tag val)

warn :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
warn tag val = Console.warn (message Yellow tag val)

error :: forall m a. MonadEffect m => Show a => String -> a -> m Unit
error tag val = Console.error (message Red tag val)

message :: forall a. Show a => Color -> String -> a -> String
message color tag val =
  colorToString color
    ++ colorTag color
    ++ " - "
    ++ tag
    ++ ": "
    ++ show val
    ++ reset

data Color
  = Red
  | Yellow
  | Cyan
  | Magenta

colorTag :: Color -> String
colorTag = case _ of
  Red -> "ERROR"
  Yellow -> "WARN"
  Cyan -> "INFO"
  Magenta -> "DEBUG"

colorToString :: Color -> String
colorToString = case _ of
  Red -> "\x1b[31m"
  Yellow -> "\x1b[33m"
  Cyan -> "\x1b[36m"
  Magenta -> "\x1b[35m"

reset :: String
reset = "\x1b[0m"
