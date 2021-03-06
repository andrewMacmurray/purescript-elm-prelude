module Elm.Task
  ( Handle
  , JSError
  , Promise
  , Response
  , Task
  , after
  , andThen
  , andThenDo
  , attempt
  , attemptWith
  , effect
  , fail
  , fromAff
  , fromAffWith
  , fromPromise
  , fromPromiseWith
  , fromResult
  , map
  , map2
  , map3
  , map4
  , map5
  , map6
  , mapError
  , onError
  , parallel2
  , parallel3
  , parallel4
  , parallel5
  , parallel6
  , peek
  , perform
  , sequence
  , succeed
  , toAff
  , toAffWith
  , toPromise
  ) where

import Elm.Basics
import Control.Monad.Error.Class (class MonadThrow)
import Control.Promise as Promise
import Data.Either as Either
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, launchAff_, runAff_)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Elm.Array as Array
import Elm.Internal.Shortcut as Shortcut
import Elm.Json (class EncodeJson)
import Elm.Json as Json
import Elm.Logger as Logger
import Elm.Result (Result(..))
import Elm.Result as Result
import Prelude (Unit, bind, pure, discard, (>>=))
import Prelude as Prelude

-- Task
newtype Task x a
  = Task (Aff (Result x a))

-- Instances
instance monadThrowTask :: MonadThrow x (Task x) where
  throwError = fail

instance monadAffTask :: MonadAff (Task x) where
  liftAff aff = Task (Prelude.map Ok aff)

instance monadEffectTask :: MonadEffect (Task x) where
  liftEffect = effect

instance monadTask :: Prelude.Monad (Task x)

instance bindTask :: Prelude.Bind (Task x) where
  bind (Task task) f = Task (task >>= onResult_)
    where
    onResult_ res = case res of
      Ok a -> toAff (f a)
      Err x -> pure (Err x)

instance applicativeTask :: Prelude.Applicative (Task x) where
  pure a = Task (pure (Ok a))

instance applyTask :: Prelude.Apply (Task x) where
  apply (Task func) (Task task) =
    Task
      ( let
          onResult_ f next = case f, next of
            (Ok f_), (Ok next_) -> Ok (f_ next_)
            (Err x), _ -> Err x
            _, (Err x) -> Err x
        in
          do
            f <- func
            t <- task
            pure (onResult_ f t)
      )

instance functorTask :: Prelude.Functor (Task x) where
  map f (Task task) = Task (task >>= (Result.map f >> pure))

type JSError
  = Aff.Error

type Handle x a
  = { onResult :: Result x a -> Effect Unit
    , onJSError :: JSError -> Effect Unit
    }

attemptWith :: forall x a. Handle x a -> Task x a -> Effect Unit
attemptWith handle (Task task) = runAff_ handle_ task
  where
  handle_ res = case res of
    Either.Left err -> handle.onJSError err
    Either.Right res_ -> handle.onResult res_

attempt :: forall x a. (Result x a -> Effect Unit) -> Task x a -> Effect Unit
attempt handle =
  attemptWith
    { onResult: handle
    , onJSError: Logger.error "JS ERROR"
    }

perform :: forall a. (a -> Effect Unit) -> Task Never a -> Effect Unit
perform handle (Task task) =
  launchAff_
    ( do
        res <- task
        case res of
          Err e_ -> liftEffect (handle (never e_))
          Ok a_ -> liftEffect (handle a_)
    )

peek :: forall x a. (Result x a -> Effect Unit) -> Task x a -> Task x a
peek f (Task task) =
  Task
    ( do
        res <- task
        liftEffect (f res)
        pure res
    )

after :: forall x a. Effect Unit -> Task x a -> Task x a
after eff task = do
  res <- task
  effect eff
  pure res

succeed :: forall x a. a -> Task x a
succeed = pure

fail :: forall x a. x -> Task x a
fail x = Task (pure (Err x))

fromResult :: forall x a. Result x a -> Task x a
fromResult = Task << pure

map :: forall x a b. (a -> b) -> Task x a -> Task x b
map = Shortcut.map

map2 :: forall x a b result. (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 = Shortcut.map2

map3 :: forall x a b c result. (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 = Shortcut.map3

map4 :: forall x a b c d result. (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 = Shortcut.map4

map5 :: forall x a b c d e result. (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 = Shortcut.map5

map6 :: forall x a b c d e f result. (a -> b -> c -> d -> e -> f -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x result
map6 = Shortcut.map6

andThen :: forall x a b. (a -> Task x b) -> Task x a -> Task x b
andThen = Shortcut.andThen

andThenDo :: forall x a b. Task x b -> Task x a -> Task x b
andThenDo task = andThen (\_ -> task)

sequence :: forall x a. Array (Task x a) -> Task x (Array a)
sequence = Array.foldr (Shortcut.map2 Array.cons) (succeed [])

onError :: forall x y a. (x -> Task y a) -> Task x a -> Task y a
onError f (Task task) = Task (task >>= onError_)
  where
  onError_ res = case res of
    Ok a -> pure (Ok a)
    Err x -> toAff (f x)

mapError :: forall x y a. (x -> y) -> Task x a -> Task y a
mapError f = onError (f >> fail)

parallel2 :: forall x a b c. (a -> b -> c) -> Task x a -> Task x b -> Task x c
parallel2 f (Task a) (Task b) = do
  Task
    ( do
        fiberA <- forkAff a
        fiberB <- forkAff b
        resA <- joinFiber fiberA
        resB <- joinFiber fiberB
        pure (Shortcut.map2 f resA resB)
    )

parallel3 :: forall x a b c d. (a -> b -> c -> d) -> Task x a -> Task x b -> Task x c -> Task x d
parallel3 f (Task a) (Task b) (Task c) = do
  Task
    ( do
        fiberA <- forkAff a
        fiberB <- forkAff b
        fiberC <- forkAff c
        resA <- joinFiber fiberA
        resB <- joinFiber fiberB
        resC <- joinFiber fiberC
        pure (Shortcut.map3 f resA resB resC)
    )

parallel4 :: forall x a b c d e. (a -> b -> c -> d -> e) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e
parallel4 f (Task a) (Task b) (Task c) (Task d) = do
  Task
    ( do
        fiberA <- forkAff a
        fiberB <- forkAff b
        fiberC <- forkAff c
        fiberD <- forkAff d
        resA <- joinFiber fiberA
        resB <- joinFiber fiberB
        resC <- joinFiber fiberC
        resD <- joinFiber fiberD
        pure (Shortcut.map4 f resA resB resC resD)
    )

parallel5 :: forall x a b c d e f. (a -> b -> c -> d -> e -> f) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f
parallel5 f (Task a) (Task b) (Task c) (Task d) (Task e) = do
  Task
    ( do
        fiberA <- forkAff a
        fiberB <- forkAff b
        fiberC <- forkAff c
        fiberD <- forkAff d
        fiberE <- forkAff e
        resA <- joinFiber fiberA
        resB <- joinFiber fiberB
        resC <- joinFiber fiberC
        resD <- joinFiber fiberD
        resE <- joinFiber fiberE
        pure (Shortcut.map5 f resA resB resC resD resE)
    )

parallel6 :: forall x a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x f -> Task x g
parallel6 f (Task a) (Task b) (Task c) (Task d) (Task e) (Task f_) = do
  Task
    ( do
        fiberA <- forkAff a
        fiberB <- forkAff b
        fiberC <- forkAff c
        fiberD <- forkAff d
        fiberE <- forkAff e
        fiberF <- forkAff f_
        resA <- joinFiber fiberA
        resB <- joinFiber fiberB
        resC <- joinFiber fiberC
        resD <- joinFiber fiberD
        resE <- joinFiber fiberE
        resF <- joinFiber fiberF
        pure (Shortcut.map6 f resA resB resC resD resE resF)
    )

effect :: forall x a. Effect a -> Task x a
effect = liftEffect >> fromAff

fromAff :: forall x a. Aff a -> Task x a
fromAff = fromAffWith Ok

fromAffWith :: forall x a b. (a -> Result x b) -> Aff a -> Task x b
fromAffWith toResult aff = Task (Prelude.map toResult aff)

type Promise :: forall k1 k2. k1 -> k2 -> Type
type Promise x a
  = Promise.Promise (Response x a)

type Response :: forall k1 k2. k1 -> k2 -> Type
type Response x a
  = { data :: Json.Value
    , error :: Json.Value
    }

toPromise :: forall x a. EncodeJson x => EncodeJson a => Task x a -> Effect (Promise x a)
toPromise (Task task) =
  Promise.fromAff
    ( do
        res <- task
        case res of
          Ok a -> pure { data: Json.encode a, error: Json.null }
          Err x -> pure { data: Json.null, error: Json.encode x }
    )

fromPromise :: forall x a. Effect (Promise.Promise a) -> Task x a
fromPromise = Promise.toAffE >> fromAff

fromPromiseWith :: forall x a b. (a -> Result x b) -> Effect (Promise.Promise a) -> Task x b
fromPromiseWith f = Promise.toAffE >> fromAffWith f

toAffWith :: forall x a b. (Result x a -> b) -> Task x a -> Aff b
toAffWith f (Task t) = Prelude.map f t

toAff :: forall x a. Task x a -> Aff (Result x a)
toAff = toAffWith Prelude.identity
