module ArrayExtra exposing (dict)

import Array exposing (Array)
import Dict exposing (Dict)


dict : Array ( comparable, a ) -> Dict comparable a
dict =
    Array.foldl (uncurry Dict.insert) Dict.empty
