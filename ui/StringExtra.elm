module StringExtra exposing (..)

import Either exposing (Either(..))
import Interval exposing (Interval)
import Intervals exposing (Intervals)
import Regex exposing (regex)


containsAll : List String -> String -> Bool
containsAll xs s =
    case xs of
        [] ->
            True

        y :: ys ->
            String.contains y s && containsAll ys s


interval : String -> String -> Maybe (Interval Int)
interval needle haystack =
    if String.isEmpty needle then
        Debug.crash <| "String.interval: empty needle"
    else
        case Regex.find (Regex.AtMost 1) (Regex.caseInsensitive (regex needle)) haystack of
            [] ->
                Nothing

            { index } :: _ ->
                Just (Interval.singleton index (index + String.length needle))


{-| Explode a string into segments that don't match the given intervals (Left)
and segments that do (Right).
-}
explode : Intervals Int -> String -> List (Either String String)
explode xs0 s =
    let
        go : Int -> List ( Int, Int ) -> List (Either String String)
        go n xs =
            case xs of
                [] ->
                    if n == String.length s then
                        []
                    else
                        [ Left (String.slice n (String.length s) s) ]

                ( x, y ) :: ys ->
                    if n < x then
                        Left (String.slice n x s)
                            :: Right (String.slice x y s)
                            :: go y ys
                    else
                        Right (String.slice x y s)
                            :: go y ys
    in
    go 0 (Intervals.toList xs0)


{-| Like words, but fixed - doesn't return singleton list of the empty string
if there are no words given.
-}
words_ : String -> List String
words_ s =
    if String.isEmpty s then
        []
    else
        String.words s
