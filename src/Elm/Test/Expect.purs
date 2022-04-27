module Elm.Test.Expect
  ( all
  , equals
  , fail
  , module Exception.Exports
  , module MonadError.Exports
  , task
  , taskEquals
  , taskOk
  , task_
  ) where

import Elm.Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Class (class MonadThrow) as MonadError.Exports
import Data.Traversable (sequence_)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Effect.Exception (Error) as Exception.Exports
import Elm.Array as Array
import Elm.Task as Task
import Test.Spec.Assertions as Assertions

fail :: forall m. MonadThrow Error m => String -> m Unit
fail = Assertions.fail

equals :: forall m a. MonadThrow Error m => Show a => Eq a => a -> a -> m Unit
equals = Assertions.shouldEqual

all :: forall m a. MonadThrow Error m => Show a => Array (a -> m Unit) -> a -> m Unit
all assertions a =
  assertions
    |> Array.map (\exp -> exp a)
    |> sequence_

taskOk :: forall x a. Show x => Show a => (a -> Aff Unit) -> Task x a -> Aff Unit
taskOk assert =
  task
    ( case _ of
        Ok a -> assert a
        Err e -> fail ("Expected Ok value, got Err: " ++ show e)
    )

task :: forall x a. Show x => Show a => (Result x a -> Aff Unit) -> Task x a -> Aff Unit
task assertion t = do
  res <- Task.unwrap_ t
  assertion res

task_ :: forall a. Show a => (a -> Aff Unit) -> Task Never a -> Aff Unit
task_ assertion t = do
  res <- Task.unwrap_ t
  case res of
    Ok a -> assertion a
    Err e -> pure (never e)

taskEquals :: forall x a. Show x => Show a => Eq x => Eq a => Result x a -> Task x a -> Aff Unit
taskEquals expected t = do
  res <- Task.unwrap_ t
  equals expected res
