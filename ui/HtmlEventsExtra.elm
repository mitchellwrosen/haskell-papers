module HtmlEventsExtra exposing (onEnter)

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode


onEnter : a -> Attribute a
onEnter x =
    Html.Events.on "keyup"
        (Html.Events.keyCode
            |> Decode.andThen
                (\code ->
                    if code == 13 then
                        Decode.succeed x
                    else
                        Decode.fail ""
                )
        )
