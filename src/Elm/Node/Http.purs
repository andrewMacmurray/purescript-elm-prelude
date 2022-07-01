module Elm.Node.Http
  ( get
  , module Http.Exports
  , post
  , request
  ) where

import Elm.Prelude
import Affjax.Node as Node
import Elm.Http (BadStatus_, Body, Error(..), Expect, GetOptions, Header, PostOptions, RequestOptions, authorization, bearerToken, contentType, customError, emptyBody, expectJson, expectString, fail, formUrlHeader, header, jsonBody, jsonError, stringBody) as Http.Exports
import Elm.Http as Http
import Elm.Json (class DecodeJson)

get :: forall a. DecodeJson a => Http.GetOptions -> Task Http.Error a
get = Http.get Node.driver

post :: forall a. DecodeJson a => Http.PostOptions -> Task Http.Error a
post = Http.post Node.driver

request :: forall a. DecodeJson a => Http.RequestOptions -> Task Http.Error a
request = Http.request Node.driver
