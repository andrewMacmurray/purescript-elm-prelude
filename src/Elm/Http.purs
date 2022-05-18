module Elm.Http
  ( BadStatus_
  , Body
  , Error(..)
  , Expect
  , GetOptions
  , Header
  , PostOptions
  , RequestOptions
  , authorization
  , bearerToken
  , contentType
  , customError
  , emptyBody
  , expectJson
  , expectString
  , fail
  , formUrlHeader
  , get
  , header
  , jsonBody
  , jsonError
  , post
  , request
  , stringBody
  ) where

import Elm.Prelude
import Affjax (Error, Request, Response, defaultRequest, printError, request) as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Either as Either
import Data.HTTP.Method (Method(..))
import Elm.Array as Array
import Elm.Json (class DecodeJson, class EncodeJson)
import Elm.Json as Json
import Elm.Result as Result
import Elm.Task as Task

type GetOptions
  = { url :: String
    , headers :: Array Header
    , expect :: Expect
    }

type PostOptions
  = { url :: String
    , headers :: Array Header
    , body :: Body
    , expect :: Expect
    }

type RequestOptions
  = { url :: String
    , headers :: Array Header
    , body :: Maybe Body
    , method :: Method
    , expect :: Expect
    }

newtype Header
  = Header RequestHeader

data Expect
  = ExpectString
  | ExpectJson

data Body
  = EmptyBody
  | StringBody String
  | JsonBody Json.Value

data Error
  = CustomError String
  | JsonError Json.Error
  | BadStatus BadStatus_
  | AffjaxError Affjax.Error

type BadStatus_
  = { status :: StatusCode
    , message :: String
    , body :: String
    }

emptyBody :: Body
emptyBody = EmptyBody

stringBody :: String -> Body
stringBody = StringBody

jsonBody :: forall a. EncodeJson a => a -> Body
jsonBody = Json.encode >> JsonBody

jsonError :: Json.Error -> Error
jsonError = JsonError

customError :: String -> Error
customError = CustomError

expectString :: Expect
expectString = ExpectString

expectJson :: Expect
expectJson = ExpectJson

formUrlHeader :: Header
formUrlHeader = contentType "application/x-www-form-urlencoded"

bearerToken :: String -> Header
bearerToken token = authorization ("Bearer " ++ token)

authorization :: String -> Header
authorization = header "Authorization"

contentType :: String -> Header
contentType = header "Content-type"

header :: String -> String -> Header
header name val = Header (RequestHeader name val)

get :: forall a. DecodeJson a => GetOptions -> Task Error a
get options =
  request
    { url: options.url
    , headers: options.headers
    , body: Nothing
    , expect: options.expect
    , method: GET
    }

post :: forall a. DecodeJson a => PostOptions -> Task Error a
post options =
  request
    { url: options.url
    , headers: options.headers
    , body: Just options.body
    , expect: options.expect
    , method: POST
    }

request :: forall a. DecodeJson a => RequestOptions -> Task Error a
request options =
  ( case options.body of
      Just b ->
        Affjax.defaultRequest
          { headers = toHeaders options.headers
          , url = options.url
          , content = toContentBody b
          , responseFormat = ResponseFormat.string
          , method = Either.Left options.method
          }
      Nothing ->
        Affjax.defaultRequest
          { headers = toHeaders options.headers
          , url = options.url
          , responseFormat = ResponseFormat.string
          , method = Either.Left options.method
          }
  )
    |> doRequest
    |> Task.andThen (handleResponse options)

toContentBody :: Body -> Maybe RequestBody
toContentBody = case _ of
  EmptyBody -> Nothing
  StringBody s -> Just (RequestBody.string s)
  JsonBody b -> Just (RequestBody.json b)

fail :: forall a. String -> Task Error a
fail = Task.fail << CustomError

doRequest :: Affjax.Request String -> Task Error (Affjax.Response String)
doRequest =
  Affjax.request
    >> Task.fromAffWith Result.fromEither
    >> Task.mapError AffjaxError

handleResponse :: forall a b. DecodeJson b => { expect :: Expect | a } -> Affjax.Response String -> Task Error b
handleResponse options res = do
  if res.status >= StatusCode 400 then do
    Task.fail
      ( BadStatus
          { status: res.status
          , message: res.statusText
          , body: res.body
          }
      )
  else case options.expect of
    ExpectString -> case Json.parse ("\"" ++ res.body ++ "\"") of
      Result.Ok b -> Task.succeed b
      Result.Err e -> Task.fail (JsonError e)
    ExpectJson -> case Json.parse res.body of
      Result.Ok b -> Task.succeed b
      Result.Err e -> Task.fail (JsonError e)

toHeaders :: Array Header -> Array RequestHeader
toHeaders = Array.map (\(Header h) -> h)

instance encodeError :: EncodeJson Error where
  encodeJson e = case e of
    CustomError e_ -> Json.encode e_
    JsonError e_ -> Json.encode e_
    BadStatus s -> Json.encode (printBadStatus s)
    AffjaxError e_ -> Json.encode (Affjax.printError e_)

instance showError :: Show Error where
  show = case _ of
    CustomError e_ -> e_
    JsonError e_ -> show e_
    BadStatus s -> printBadStatus s
    AffjaxError e_ -> Affjax.printError e_

instance eqError :: Eq Error where
  eq e e' = eq (show e) (show e')

printBadStatus :: BadStatus_ -> String
printBadStatus s =
  "Bad Status "
    ++ show s.status
    ++ ": "
    ++ s.message
    ++ ": "
    ++ s.body

instance showHeader :: Show Header where
  show (Header h) = show h
