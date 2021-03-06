module Intersection
    exposing
        ( Intersection
        , append
        , empty
        , fromSet
        , member
        , toSet
        , unsafeToSet
        )

import Set exposing (Set)


type Intersection a
    = Empty
    | Full (Set a)


empty : Intersection a
empty =
    Empty


fromSet : Set comparable -> Intersection comparable
fromSet =
    Full


member : comparable -> Intersection comparable -> Bool
member x xs =
    case xs of
        Empty ->
            True

        Full ys ->
            Set.member x ys


append :
    Intersection comparable
    -> Intersection comparable
    -> Intersection comparable
append xs ys =
    case xs of
        Empty ->
            ys

        Full xs_ ->
            case ys of
                Empty ->
                    Full xs_

                Full ys_ ->
                    Full (Set.intersect xs_ ys_)


toSet : Intersection comparable -> Maybe (Set comparable)
toSet xs =
    case xs of
        Empty ->
            Nothing

        Full ys ->
            Just ys


unsafeToSet : Intersection comparable -> Set comparable
unsafeToSet xs =
    case xs of
        Empty ->
            Debug.crash "Intersection.unsafeToSet"

        Full ys ->
            ys
