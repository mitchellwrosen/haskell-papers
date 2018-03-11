module ListExtra exposing (deleteBy)


deleteBy : (a -> Bool) -> List a -> List a
deleteBy p xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if p y then
                ys
            else
                y :: deleteBy p ys
