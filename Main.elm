module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Http
import Html exposing (Html)
import Html.Attributes exposing (href, class)
import Json.Decode as Decode exposing (Decoder)


--------------------------------------------------------------------------------
-- Types and type aliases


type Model
    = Model (Array Paper)


type Message
    = Blob (Result Http.Error (Array Paper))


type alias AuthorId =
    Int


type alias Author =
    String


type alias LinkId =
    Int


type alias Link =
    String


type alias TitleId =
    Int


type alias Title =
    String


type alias Paper =
    { title : Title
    , authors : Array Author
    , year : Maybe Int
    , references : Array TitleId
    , links : Array Link
    , file : Int
    , line : Int
    }



--------------------------------------------------------------------------------
-- Boilerplate main function


main : Program Never Model Message
main =
    Html.program
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }



--------------------------------------------------------------------------------
-- Initialize the model and sennd the first message


init : ( Model, Cmd Message )
init =
    let
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

        decodePapers : Decoder (Array Paper)
        decodePapers =
            andThen3
                (Decode.field "a" decodeIds)
                (Decode.field "b" decodeIds)
                (Decode.field "c" decodeIds)
                (\titles authors links ->
                    decodePaper titles authors links
                        |> Decode.array
                        |> Decode.field "d"
                )

        decodeIds : Decoder (Dict Int String)
        decodeIds =
            Decode.map arrayToDict <|
                Decode.array <|
                    Decode.map2
                        (\x y -> ( x, y ))
                        (Decode.index 0 Decode.int)
                        (Decode.index 1 Decode.string)
    in
        ( Model Array.empty
        , Http.send Blob <| Http.get "./papers.json" decodePapers
        )


decodePaper :
    Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> Decoder Paper
decodePaper titles authors links =
    Decode.map7 Paper
        (decodeTitle titles)
        (decodeAuthors authors)
        decodeYear
        decodeReferences
        (decodeLinks links)
        decodeFile
        decodeLine


decodeAuthors : Dict AuthorId Author -> Decoder (Array Author)
decodeAuthors authors =
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
            |> Decode.field "b"
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault Array.empty)


decodeFile : Decoder Int
decodeFile =
    Decode.int
        |> Decode.field "f"


decodeLine : Decoder Int
decodeLine =
    Decode.int
        |> Decode.field "g"


decodeLinks : Dict LinkId Link -> Decoder (Array Link)
decodeLinks links =
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
            |> Decode.field "e"
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault Array.empty)


decodeReferences : Decoder (Array TitleId)
decodeReferences =
    Decode.int
        |> Decode.array
        |> Decode.field "d"
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault Array.empty)


decodeTitle : Dict TitleId Title -> Decoder Title
decodeTitle titles =
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
            |> Decode.field "a"
            |> Decode.map lookupTitle


decodeYear : Decoder (Maybe Int)
decodeYear =
    Decode.int
        |> Decode.field "c"
        |> Decode.maybe



--------------------------------------------------------------------------------
-- The main update loop


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Blob (Ok papers) ->
            ( Model papers
            , Cmd.none
            )

        Blob (Err msg) ->
            Debug.crash <| toString msg



--------------------------------------------------------------------------------
-- Render HTML


view : Model -> Html Message
view model =
    Html.div [ class "container" ]
        [ Html.header []
            [ Html.h1 [] [ Html.text "Haskell Papers" ]
            , Html.a [ class "subtle-link", href "https://github.com/mitchellwrosen/haskell-papers" ] [ Html.text "contribute on GitHub" ]
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
        , viewDetails paper
        , viewEditLink paper
        ]


viewTitle : Paper -> Html a
viewTitle paper =
    Html.p [ class "title" ]
        [ case Array.get 0 paper.links of
            Nothing ->
                Html.text paper.title

            Just link ->
                Html.a [ class "link", href link ] [ Html.text paper.title ]
        ]


viewEditLink : Paper -> Html a
viewEditLink paper =
    let
        editLink : String
        editLink =
            "https://github.com/mitchellwrosen/haskell-papers/edit/master/papers"
                ++ String.padLeft 3 '0' (toString paper.file)
                ++ ".yaml#L"
                ++ toString paper.line
    in
        Html.a [ class "subtle-link edit", href editLink ] [ Html.text "(edit)" ]


viewDetails : Paper -> Html a
viewDetails paper =
    Html.p [ class "details" ]
        [ paper.authors |> Array.toList |> String.join ", " |> Html.text
        , case paper.year of
            Nothing ->
                Html.text ""

            Just year ->
                Html.text (" [" ++ toString year ++ "]")
        ]



--------------------------------------------------------------------------------
-- Misc. utility functions


arrayToDict : Array ( comparable, a ) -> Dict comparable a
arrayToDict =
    Array.foldl (uncurry Dict.insert) Dict.empty
