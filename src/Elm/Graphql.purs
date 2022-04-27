module Elm.Graphql
  ( Endpoint
  , Gql(..)
  , GraphQL
  , Client
  , Client'
  , class Operation
  , request
  ) where

import Elm.Prelude
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Elm.Http as Http
import Elm.Json (class DecodeJson, class EncodeJson)
import Elm.Json as Json
import Elm.Task as Task
import Type.Proxy (Proxy(..))

data GraphQL

data Gql (operation :: GraphQL)
  = Gql

class Operation (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type) | operation -> gql i o

type Endpoint
  = String

type Client
  = forall (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type). Client' operation gql i o

type Client' (operation :: GraphQL) (gql :: Symbol) (i :: Row Type) (o :: Row Type)
  = Operation operation gql i o =>
    IsSymbol gql =>
    EncodeJson { | i } =>
    DecodeJson { | o } =>
    Gql operation -> Record i -> Task Http.Error { | o }

request :: Endpoint -> Array Http.Header -> Client
request endpoint headers = go
  where
  go ::
    forall (operation :: GraphQL) (i :: Row Type) (o :: Row Type) (gql :: Symbol).
    Operation operation gql i o =>
    IsSymbol gql =>
    EncodeJson { | i } =>
    DecodeJson { | o } =>
    Gql operation -> Record i -> Task Http.Error { | o }
  go _ variables = do
    let
      input =
        { variables
        , query:
            String.replaceAll
              (String.Pattern "\n")
              (String.Replacement " ")
              ( String.replaceAll
                  (String.Pattern "\r\n")
                  (String.Replacement " ")
                  (reflectSymbol (Proxy :: Proxy gql))
              )
        }
    ( Http.post
        { url: endpoint
        , headers: headers ++ [ Http.contentType "application/json" ]
        , body: Http.stringBody (Json.toString input)
        , expect: Http.expectJson
        } ::
        Task Http.Error Json.Value
    )
      |> Task.andThen
          ( \res -> case (Json.readValue res) :: Result Json.Error { data :: { | o } } of
              Ok x -> Task.succeed x.data
              Err e ->
                Task.fail
                  ( Http.customError
                      ( String.joinWith "\n"
                          [ "Error Decoding Graphql Response:"
                          , show e
                          , "Response:"
                          , Json.toString res
                          ]
                      )
                  )
          )
