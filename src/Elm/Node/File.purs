module Elm.Node.File
  ( Path
  , read
  , write
  ) where

import Control.Promise (Promise)
import Effect (Effect)
import Elm.Basics ((>>))
import Elm.Task (Task)
import Elm.Task as Task
import Prelude (Unit)

type Path
  = String

read :: forall err. Path -> Task err String
read = read_ >> Task.fromPromise

write :: forall err. Path -> String -> Task err Unit
write path = write_ path >> Task.fromPromise

foreign import write_ :: String -> String -> Effect (Promise Unit)

foreign import read_ :: String -> Effect (Promise String)
