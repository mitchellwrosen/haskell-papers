module ArrayExtra exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import MaybeExtra as Maybe
import Set exposing (Set)


toDict : Array ( comparable, a ) -> Dict comparable a
toDict =
    Array.foldl (uncurry Dict.insert) Dict.empty


toSet : Array comparable -> Set comparable
toSet =
    Array.foldl Set.insert Set.empty


{-| Like 'get', but modulo the given int by the length of the array. Only
returns 'Nothing' if the given 'Array' is empty.
-}
getCycle : Int -> Array a -> Maybe a
getCycle n xs =
    if Array.isEmpty xs then
        Nothing
    else
        Array.get (n % Array.length xs) xs
