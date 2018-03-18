module BasicsExtra exposing (..)


apply : a -> (a -> b) -> b
apply x f =
    f x


equals : a -> a -> Bool
equals =
    (==)
