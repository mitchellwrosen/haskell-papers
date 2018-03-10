port module NoUiSlider
    exposing
        ( NoUiSliderCreate
        , noUiSliderCreate
        )

{-| Elm interface to <https://refreshless.com/nouislider/>

This module is designed to be imported unqualified, because the ports here get
associated handlers in 'nouislider-shim.js' with the same names.

-}


type alias NoUiSliderCreate =
    { id : String -- The element id to create a slider in
    , start : List Int -- The starting handles' starting positions
    , margin : Maybe Int -- How far apart *must* handles be?
    , limit : Maybe Int -- How far apart *may* handles be?
    , connect : Maybe Bool -- Display a colored bar between the handles?
    , direction : Maybe String -- Not sure what this is
    , orientation : Maybe String
    , behavior : Maybe String
    , step : Maybe Int
    , tooltips : Maybe Bool
    , range : Maybe { min : Int, max : Int }
    , pipes : Maybe { mode : String, stepped : Bool, density : Int }
    }


{-| Create a slider.
-}
port noUiSliderCreate : NoUiSliderCreate -> Cmd a
