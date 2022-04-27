module Elm.Node.Env
  ( optional
  , unsafe
  , unsafeInt
  ) where

optional :: String -> String -> String
optional = optional_

unsafe :: String → String
unsafe = unsafe_

unsafeInt :: String -> Int
unsafeInt = unsafeInt_

foreign import unsafe_ :: String -> String

foreign import unsafeInt_ :: String -> Int

foreign import optional_ :: String -> String -> String
