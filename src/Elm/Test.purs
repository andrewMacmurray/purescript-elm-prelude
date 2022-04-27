module Elm.Test
  ( Test
  , beforeEach
  , module Spec.Exports
  , run
  ) where

import Elm.Prelude
import Effect.Aff (launchAff_)
import Elm.Task as Task
import Test.Spec (Spec)
import Test.Spec (describe, it) as Spec.Exports
import Test.Spec as Spec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

type Test
  = Spec Unit

run :: Test -> Effect Unit
run tests = launchAff_ (runSpec [ consoleReporter ] tests)

beforeEach :: forall x a. Task x a -> Test -> Test
beforeEach task =
  Spec.before
    ( do
        _ <- Task.unwrap_ task
        pure unit
    )
