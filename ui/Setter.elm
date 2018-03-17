module Setter exposing (..)


type alias Setter s t a b =
    (a -> b) -> s -> t


over : Setter s t a b -> (a -> b) -> s -> t
over =
    identity


fst : Setter ( a, x ) ( b, x ) a b
fst f ( a, x ) =
    ( f a, x )
