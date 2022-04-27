# purescript-elm-prelude

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
