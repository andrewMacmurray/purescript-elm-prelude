# Purescript Elm Prelude

An Elm inspired prelude for PureScript

Replace:

```purescript
import Prelude
```

With:

```purescript
import Elm.Prelude
```

And get a more Elm like experience:

```purescript
Ok 3
  |> Result.map (add 5)
  |> Result.toMaybe
  |> Maybe.withDefault 1
```

## CAVEATS

- it's very much WIP (the Api hasn't settled and there are missing modules and functions for `Dict`, `Set`, `String` etc)
- it's not published on Pursuit

## Installing

If you want to use it in your project add to your `packages.dhall`:

```dhall
let upstream =
    <your package set url>

in  upstream
  with elm-prelude =
    { dependencies =
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
    , repo = "https://github.com/andrewMacmurray/purescript-elm-prelude.git"
    , version = "main" -- or add the git sha of the commit you'd like to use
    }
```

add add it to your `spago.dhall`:

```dhall
{ name = "my-project"
, dependencies = [ "elm-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
```
