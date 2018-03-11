module JsonDecodeExtra
    exposing
        ( andThen3
        , intArray
        , intField
        , optField
        , optIntField
        , tuple2
        , withDefault
        )

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)


andThen3 :
    Decoder a
    -> Decoder b
    -> Decoder c
    -> (a -> b -> c -> Decoder d)
    -> Decoder d
andThen3 dx dy dz f =
    Decode.andThen
        (\x ->
            Decode.andThen
                (\y ->
                    Decode.andThen (f x y) dz
                )
                dy
        )
        dx


intArray : Decoder (Array Int)
intArray =
    Decode.array Decode.int


intField : String -> Decoder Int
intField s =
    Decode.field s Decode.int


optField : String -> Decoder a -> Decoder (Maybe a)
optField s x =
    Decode.field s x
        |> Decode.maybe


optIntField : String -> Decoder (Maybe Int)
optIntField s =
    optField s Decode.int


tuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
tuple2 dx dy =
    Decode.map2
        (\x y -> ( x, y ))
        (Decode.index 0 dx)
        (Decode.index 1 dy)


withDefault : a -> Decoder a -> Decoder a
withDefault x =
    Decode.maybe
        >> Decode.map (Maybe.withDefault x)
