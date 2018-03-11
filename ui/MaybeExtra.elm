module MaybeExtra exposing (guard)


guard : Bool -> a -> Maybe a
guard b =
    if b then
        Just
    else
        always Nothing
