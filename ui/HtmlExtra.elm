module HtmlExtra exposing (empty, thunk)

import Html exposing (Html)
import Html.Lazy as Html


empty : Html a
empty =
    Html.text ""


thunk : Html a -> Html a
thunk html =
    Html.lazy (always html) unit


{-| For reference-equality of 'thunk'
-}
unit : ()
unit =
    ()
