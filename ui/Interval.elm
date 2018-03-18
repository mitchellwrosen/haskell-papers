module Interval exposing (..)


type Interval a
    = Interval a a


singleton : comparable -> comparable -> Interval comparable
singleton x y =
    if x >= y then
        Debug.crash <| "Interval.singleton (" ++ toString x ++ ", " ++ toString y ++ ")"
    else
        Interval x y
