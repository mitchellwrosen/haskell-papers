module RegexExtra exposing (..)

import Regex exposing (Regex)


containsAll : List Regex -> String -> Bool
containsAll rs s =
    case rs of
        [] ->
            True

        r :: rs ->
            Regex.contains r s && containsAll rs s
