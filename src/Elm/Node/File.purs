module Elm.Node.File
  ( Path
  , read
  , readJson
  , write
  ) where

import Control.Promise (Promise)
import Effect (Effect)
import Elm.Basics ((>>))
import Elm.Json as Json
import Elm.Task (Task)
import Elm.Task as Task
import Prelude (Unit)

type Path
  = String

readJson :: forall a. Json.DecodeJson a => Path -> Task Json.Error a
readJson = read_ >> Task.fromPromiseWith Json.parse

read :: forall err. Path -> Task err String
read = read_ >> Task.fromPromise

write :: forall err. Path -> String -> Task err Unit
write path = write_ path >> Task.fromPromise

foreign import write_ :: String -> String -> Effect (Promise Unit)

foreign import read_ :: String -> Effect (Promise String)
