module Elm.Node.File
  ( Path
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

write :: forall err. Path -> String -> Task err Unit
write filePath = write_ filePath >> Task.fromPromise

foreign import write_ :: String -> String -> Effect (Promise Unit)
