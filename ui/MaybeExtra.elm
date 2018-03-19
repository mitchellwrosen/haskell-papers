module MaybeExtra exposing (..)


fromJust : Maybe a -> a
fromJust x =
    case x of
        Nothing ->
            Debug.crash "Maybe.fromJust Nothing"

        Just y ->
            y


guard : Bool -> a -> Maybe a
guard b =
    if b then
        Just
    else
        always Nothing
