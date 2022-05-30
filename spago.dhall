{ name = "purescript-elm-prelude"
, dependencies =  
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "http-methods"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
