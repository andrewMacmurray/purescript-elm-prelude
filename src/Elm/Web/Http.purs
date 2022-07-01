module Elm.Web.Http
  ( get
  , module Http.Exports
  , post
  , request
  ) where

import Elm.Prelude
import Affjax.Web as Web
import Elm.Http (BadStatus_, Body, Error(..), Expect, GetOptions, Header, PostOptions, RequestOptions, authorization, bearerToken, contentType, customError, emptyBody, expectJson, expectString, fail, formUrlHeader, header, jsonBody, jsonError, stringBody) as Http.Exports
import Elm.Http as Http
import Elm.Json (class DecodeJson)

get :: forall a. DecodeJson a => Http.GetOptions -> Task Http.Error a
get = Http.get Web.driver

post :: forall a. DecodeJson a => Http.PostOptions -> Task Http.Error a
post = Http.post Web.driver

request :: forall a. DecodeJson a => Http.RequestOptions -> Task Http.Error a
request = Http.request Web.driver
