module Elm.Node.Env
  ( int
  , optionalInt
  , optionalString
  , string
  ) where

optionalString :: String -> String -> String
optionalString = optionalString_

optionalInt :: String -> Int -> Int
optionalInt = optionalInt_

string :: String -> String
string = string_

int :: String -> Int
int = int_

foreign import string_ :: String -> String

foreign import optionalString_ :: String -> String -> String

foreign import int_ :: String -> Int

foreign import optionalInt_ :: String -> Int -> Int
