module ArrayExtra
  exposing
    (toDict,
    toSet)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


toDict : Array ( comparable, a ) -> Dict comparable a
toDict =
    Array.foldl (uncurry Dict.insert) Dict.empty

toSet : Array comparable -> Set comparable
toSet =
    Array.foldl Set.insert Set.empty
