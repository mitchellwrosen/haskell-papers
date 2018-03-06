module Main exposing (..)

import Dict exposing (Dict)
import Http
import Html exposing (Html)
import Html.Attributes exposing (href)
import Json.Decode as Decode exposing (Decoder)


type Model
    = Model (Dict Int Paper) (List Paper)


type Message
    = Blob (Result Http.Error (List ( Int, Paper )))


type alias Paper =
    { name : String
    , links : List String
    , references : List Int
    }


main : Program Never Model Message
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Message )
init =
    let
        also : Decoder a -> Decoder (a -> b) -> Decoder b
        also =
            Decode.map2 (|>)

        decodePapers : Decoder (List ( Int, Paper ))
        decodePapers =
            Decode.list <|
                Decode.map2
                    (\x y -> ( x, y ))
                    (Decode.index 0 Decode.int)
                    (Decode.index 1 decodePaper)

        decodePaper : Decoder Paper
        decodePaper =
            Decode.succeed Paper
                |> also (Decode.field "name" Decode.string)
                |> also (Decode.field "links" (Decode.list Decode.string))
                |> also (Decode.field "references" (Decode.list Decode.int))
    in
        ( Model Dict.empty []
        , Http.send Blob <| Http.get "./papers.json" decodePapers
        )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Blob (Ok papers) ->
            ( Model (listToDict papers) (List.map Tuple.second papers)
            , Cmd.none
            )

        Blob (Err msg) ->
            Debug.crash <| toString msg


listToDict : List ( comparable, a ) -> Dict comparable a
listToDict =
    List.foldl (uncurry Dict.insert) Dict.empty


view : Model -> Html Message
view model =
    case model of
        Model papersMap papers ->
            Html.ul [] <| List.map (viewPaper papersMap) papers


viewPaper : Dict Int Paper -> Paper -> Html a
viewPaper papers paper =
    Html.li
        []
        [ case paper.links of
            [] ->
                Html.text paper.name

            link :: _ ->
                Html.a [ href link ] [ Html.text paper.name ]
        , Html.ul
            []
            (List.map
                (\id ->
                    case Dict.get id papers of
                        Nothing ->
                            Debug.crash ("No paper id " ++ toString id)

                        Just ref ->
                            viewPaper papers ref
                )
                paper.references
            )
        ]
