module DictExtra
    exposing
        ( keysSet
        , unsafeGet
        )

import Dict exposing (Dict)
import Set exposing (Set)


{-| Collect the keys of a 'Dict' into a 'Set'
-}
keysSet : Dict comparable a -> Set comparable
keysSet =
    Dict.foldl (\k _ -> Set.insert k) Set.empty


{-| 'Dict.get', but partial. Should only be used when the given key must exist
in the Dict.
-}
unsafeGet : Dict comparable a -> comparable -> a
unsafeGet dict key =
    case Dict.get key dict of
        Nothing ->
            Debug.crash <|
                "Dict.unsafeGet: no key '"
                    ++ toString key
                    ++ "' found in dict: "
                    ++ toString dict

        Just value ->
            value
