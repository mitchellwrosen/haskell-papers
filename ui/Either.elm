module Either exposing (..)


type Either a b
    = Left a
    | Right b


either : (a -> r) -> (b -> r) -> Either a b -> r
either f g x =
    case x of
        Left y ->
            f y

        Right y ->
            g y
