module Elm.Json
  ( Decoder
  , Error(..)
  , Value
  , andThen
  , decode
  , decodeAndThen
  , decodeMap
  , decodeString
  , encode
  , errorToString
  , fail
  , map
  , module Argonaut.Exports
  , null
  , oneOf
  , parse
  , readValue
  , succeed
  , toString
  ) where

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson) as Argonaut.Exports
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut as Argonaut
import Data.Either (Either)
import Data.Either as Either
import Elm.Array as Array
import Elm.Basics ((++), (>>))
import Elm.Internal.Shortcut as Shortcut
import Elm.Result (Result)
import Elm.Result as Result
import Prelude as Prelude

type Decoder a
  = Either JsonDecodeError a

data Error
  = MalformedError String
  | DecodeError JsonDecodeError

type Value
  = Json

instance encodeError :: EncodeJson Error where
  encodeJson = errorToString >> Argonaut.encodeJson

instance showError :: Prelude.Show Error where
  show = errorToString

errorToString :: Error -> String
errorToString = case _ of
  MalformedError err -> "Malformed JSON: " ++ err
  DecodeError err -> printJsonDecodeError err

parse :: forall a. DecodeJson a => String -> Result Error a
parse =
  Argonaut.jsonParser
    >> Result.fromEither
    >> Result.mapError MalformedError
    >> Result.andThen (Argonaut.decodeJson >> Result.fromEither >> Result.mapError DecodeError)

readValue :: forall a. DecodeJson a => Value -> Result Error a
readValue = Argonaut.decodeJson >> Result.fromEither >> Result.mapError DecodeError

toString :: forall a. EncodeJson a => a -> String
toString = encode >> Argonaut.stringify

encode :: forall a. EncodeJson a => a -> Value
encode = Argonaut.encodeJson

succeed :: forall a. a -> Decoder a
succeed = Prelude.pure

null :: Value
null = Argonaut.jsonNull

decode :: forall a. DecodeJson a => Value -> Decoder a
decode = Argonaut.decodeJson

decodeString :: forall a. DecodeJson a => String -> Decoder a
decodeString s = case Argonaut.jsonParser s of
  Either.Right val -> decode val
  Either.Left err -> fail err

decodeAndThen :: forall a b. DecodeJson a => (a -> Decoder b) -> Value -> Decoder b
decodeAndThen f = decode >> andThen f

fail :: forall a. String -> Decoder a
fail reason = Either.Left (Named reason MissingValue)

map :: forall a b. (a -> b) -> Decoder a -> Decoder b
map = Shortcut.map

decodeMap :: forall a b. DecodeJson a => (a -> b) -> Json -> Decoder b
decodeMap f v = map f (decode v)

andThen :: forall a b. (a -> Decoder b) -> Decoder a -> Decoder b
andThen = Shortcut.andThen

oneOf :: forall a. Array (Decoder a) -> Decoder a
oneOf = Array.foldl (\a b -> a <|> b) (fail "No Decoder Passed")
