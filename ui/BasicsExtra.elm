module BasicsExtra exposing (..)


apply : a -> (a -> b) -> b
apply x f =
    f x


equals : a -> a -> Bool
equals =
    (==)


maybe : b -> (a -> b) -> Maybe a -> b
maybe z f x =
    case x of
        Nothing ->
            z

        Just y ->
            f y
