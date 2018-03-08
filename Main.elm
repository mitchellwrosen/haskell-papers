module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Http
import Html exposing (Html)
import Html.Attributes exposing (href, class)
import Json.Decode as Decode exposing (Decoder)


type Model
    = Model (Array Paper)


type Message
    = Blob (Result Http.Error (Array Paper))


type alias Author =
    String


type alias AuthorId =
    Int


type alias Link =
    String


type alias LinkId =
    Int


type alias Title =
    String


type alias TitleId =
    Int


type alias Paper =
    { title : Title
    , authors : Array Author
    , year : Maybe Int
    , references : Array TitleId
    , links : Array Link
    }


main : Program Never Model Message
main =
    Html.program
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : ( Model, Cmd Message )
init =
    let
        also : Decoder a -> Decoder (a -> b) -> Decoder b
        also =
            Decode.map2 (|>)

        andThen3 :
            Decoder a
            -> Decoder b
            -> Decoder c
            -> (a -> b -> c -> Decoder d)
            -> Decoder d
        andThen3 dx dy dz f =
            Decode.andThen
                (\x -> Decode.andThen (\y -> Decode.andThen (f x y) dz) dy)
                dx

        decodePapers : Decoder (Array Paper)
        decodePapers =
            andThen3
                (Decode.field "titles" decodeIds)
                (Decode.field "authors" decodeIds)
                (Decode.field "links" decodeIds)
                (\titles authors links ->
                    decodePaper titles authors links
                        |> Decode.array
                        |> Decode.field "papers"
                )

        decodeIds : Decoder (Dict Int String)
        decodeIds =
            Decode.map arrayToDict <|
                Decode.array <|
                    Decode.map2
                        (\x y -> ( x, y ))
                        (Decode.index 0 Decode.int)
                        (Decode.index 1 Decode.string)

        decodePaper : Dict TitleId Title -> Dict AuthorId Author -> Dict LinkId Link -> Decoder Paper
        decodePaper titles authors links =
            let
                decodeTitle : Decoder Title
                decodeTitle =
                    let
                        lookupTitle : TitleId -> Title
                        lookupTitle id =
                            case Dict.get id titles of
                                Nothing ->
                                    Debug.crash ("No title " ++ toString id)

                                Just title ->
                                    title
                    in
                        Decode.int
                            |> Decode.field "title"
                            |> Decode.map lookupTitle

                decodeAuthors : Decoder (Array Author)
                decodeAuthors =
                    let
                        lookupAuthor : AuthorId -> Author
                        lookupAuthor id =
                            case Dict.get id authors of
                                Nothing ->
                                    Debug.crash ("No author " ++ toString id)

                                Just author ->
                                    author
                    in
                        Decode.int
                            |> Decode.map lookupAuthor
                            |> Decode.array
                            |> Decode.field "authors"
                            |> Decode.maybe
                            |> Decode.map (Maybe.withDefault Array.empty)

                decodeYear : Decoder (Maybe Int)
                decodeYear =
                    Decode.int
                        |> Decode.field "year"
                        |> Decode.maybe

                decodeReferences : Decoder (Array TitleId)
                decodeReferences =
                    Decode.int
                        |> Decode.array
                        |> Decode.field "references"
                        |> Decode.maybe
                        |> Decode.map (Maybe.withDefault Array.empty)

                decodeLinks : Decoder (Array Link)
                decodeLinks =
                    let
                        lookupLink : LinkId -> Link
                        lookupLink id =
                            case Dict.get id links of
                                Nothing ->
                                    Debug.crash ("No link " ++ toString id)

                                Just link ->
                                    link
                    in
                        Decode.int
                            |> Decode.map lookupLink
                            |> Decode.array
                            |> Decode.field "links"
                            |> Decode.maybe
                            |> Decode.map (Maybe.withDefault Array.empty)
            in
                Decode.map5 Paper
                    decodeTitle
                    decodeAuthors
                    decodeYear
                    decodeReferences
                    decodeLinks
    in
        ( Model Array.empty
        , Http.send Blob <| Http.get "./papers.json" decodePapers
        )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Blob (Ok papers) ->
            ( Model papers
            , Cmd.none
            )

        Blob (Err msg) ->
            Debug.crash <| toString msg


arrayToDict : Array ( comparable, a ) -> Dict comparable a
arrayToDict =
    Array.foldl (uncurry Dict.insert) Dict.empty


view : Model -> Html Message
view model =
    Html.div [ class "container" ]
        [ Html.header []
            [ Html.h1 [] [ Html.text "Haskell Papers" ]
            , Html.a [ class "contribute", href "https://github.com/mitchellwrosen/haskell-papers" ] [ Html.text "contribute on GitHub" ]
            ]
        , case model of
            Model papers ->
                Html.ul [ class "paper-list" ] <| Array.toList <| Array.map viewPaper papers
        ]


viewPaper : Paper -> Html a
viewPaper paper =
    Html.li
        [ class "paper" ]
        [ viewTitle paper
        ]


viewTitle : Paper -> Html a
viewTitle paper =
    Html.p [ class "title" ]
        [ case Array.get 0 paper.links of
            Nothing ->
                Html.text paper.title

            Just link ->
                Html.a [ href link ] [ Html.text paper.title ]
        ]
