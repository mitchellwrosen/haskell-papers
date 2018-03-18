module TaskExtra exposing (..)

import Task exposing (Task)


and : Task x a -> Task x b -> Task x ( b, a )
and =
    Task.map2 (\x y -> ( y, x ))
