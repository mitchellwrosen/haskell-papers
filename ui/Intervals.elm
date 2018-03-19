module Intervals
    exposing
        ( Intervals
        , empty
        , insert
        , singleton
        , toList
        )

import Interval exposing (Interval(..))


type Intervals a
    = Intervals (List (Interval a))


empty : Intervals a
empty =
    Intervals []


singleton : Interval a -> Intervals a
singleton x =
    Intervals [ x ]


insert :
    Interval comparable
    -> Intervals comparable
    -> Intervals comparable
insert (Interval x y) (Intervals xs) =
    Intervals (insert_ x y xs)


insert_ :
    comparable
    -> comparable
    -> List (Interval comparable)
    -> List (Interval comparable)
insert_ x y xs =
    case xs of
        [] ->
            [ Interval x y ]

        (Interval r s) :: rs ->
            if y < r then
                Interval x y :: Interval r s :: rs
            else if s < x then
                Interval r s :: insert_ x y rs
            else
                Interval (min x r) (max y s) :: rs


toList : Intervals a -> List ( a, a )
toList (Intervals xs) =
    List.map (\(Interval x y) -> ( x, y )) xs
