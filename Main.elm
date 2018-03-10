module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Http
import Html exposing (Html)
import Html.Attributes exposing (href, class)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import NoUiSlider exposing (..)
import Set exposing (Set)


--------------------------------------------------------------------------------
-- Types and type aliases


type alias Model =
    { papers : Array Paper
    , titles : Dict TitleId Title
    , authors : Dict AuthorId Author
    , links : Dict LinkId Link
    , authorsIndex : Dict AuthorId (Set TitleId)
    , visible : Set TitleId
    }


type Message
    = Blob (Result Http.Error Papers)
    | AuthorsFilter String


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


type alias Papers =
    { titles : Dict TitleId Title
    , authors : Dict AuthorId Author
    , links : Dict LinkId Link
    , papers : Array Paper
    }


type alias Paper =
    { title : TitleId
    , authors : Array AuthorId
    , year : Maybe Int
    , references : Array TitleId
    , links : Array LinkId
    , file : Int
    , line : Int
    }


{-| Inverted index.
-}
type alias II a b =
    Dict a (Set b)



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

        decodePapers : Decoder Papers
        decodePapers =
            andThen3
                (Decode.field "a" decodeIds)
                (Decode.field "b" decodeIds)
                (Decode.field "c" decodeIds)
                (\titles authors links ->
                    decodePaper titles authors links
                        |> Decode.array
                        |> Decode.field "d"
                        |> Decode.map
                            (\papers ->
                                { authors = authors
                                , links = links
                                , papers = papers
                                , titles = titles
                                }
                            )
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
        ( { papers = Array.empty
          , titles = Dict.empty
          , authors = Dict.empty
          , links = Dict.empty
          , authorsIndex = Dict.empty
          , visible = Set.empty
          }
        , Http.send Blob <| Http.get "./papers.json" decodePapers
        )


decodePaper :
    Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> Decoder Paper
decodePaper titles authors links =
    Decode.map7 Paper
        (Decode.field "a" Decode.int)
        decodeAuthors
        decodeYear
        decodeReferences
        decodeLinks
        decodeFile
        decodeLine


decodeAuthors : Decoder (Array AuthorId)
decodeAuthors =
    Decode.int
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


decodeLinks : Decoder (Array LinkId)
decodeLinks =
    Decode.int
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
        Blob (Ok blob) ->
            ( { papers = blob.papers
              , titles = blob.titles
              , authors = blob.authors
              , links = blob.links
              , authorsIndex = buildAuthorsIndex blob.papers blob.authors
              , visible = dictKeysToSet blob.titles
              }
            , noUiSliderCreate
                { id = "year-slider"
                , start = [ 20, 80 ]
                , margin = Nothing
                , limit = Nothing
                , connect = Just True
                , direction = Nothing
                , orientation = Nothing
                , behavior = Nothing
                , step = Nothing
                , tooltips = Nothing
                , range = Just { min = 0, max = 100 }
                , pipes = Nothing
                }
            )

        Blob (Err msg) ->
            Debug.crash <| toString msg

        AuthorsFilter s ->
            ( applyAuthorsFilter s model, Cmd.none )


{-| Build an inverted index mapping author ids to the set of paper title ids by
that author.
-}
buildAuthorsIndex :
    Array Paper
    -> Dict AuthorId Author
    -> Dict AuthorId (Set TitleId)
buildAuthorsIndex papers authors =
    let
        step :
            Paper
            -> Dict AuthorId (Set TitleId)
            -> Dict AuthorId (Set TitleId)
        step paper dict =
            Dict.merge
                Dict.insert
                (\k v1 v2 -> Dict.insert k (Set.union v1 v2))
                Dict.insert
                (Array.foldl
                    (\id -> Dict.insert id (Set.singleton paper.title))
                    Dict.empty
                    paper.authors
                )
                dict
                Dict.empty
    in
        Array.foldl step Dict.empty papers


applyAuthorsFilter : String -> Model -> Model
applyAuthorsFilter s model =
    { model
        | visible =
            Dict.foldl
                (\id name ->
                    if fuzzyMatch (String.toLower s) (String.toLower name) then
                        case Dict.get id model.authorsIndex of
                            Nothing ->
                                identity

                            Just titles ->
                                Set.union titles
                    else
                        identity
                )
                Set.empty
                model.authors
    }



--------------------------------------------------------------------------------
-- Render HTML


view : Model -> Html Message
view model =
    Html.div [ class "container" ]
        [ Html.header []
            [ Html.h1 [] [ Html.text "Haskell Papers" ]
            , Html.a
                [ class "subtle-link"
                , href "https://github.com/mitchellwrosen/haskell-papers"
                ]
                [ Html.text "contribute on GitHub" ]
            , Html.div []
                [ Html.input [ Html.Events.onInput AuthorsFilter ] [] ]
            , Html.div
                [ Html.Attributes.id "year-slider"

                -- Uncomment me to show; hidden by default because this feature
                -- isn't finished yet.
                , Html.Attributes.style [ ( "display", "none" ) ]
                ]
                []
            ]
        , case model of
            { papers, visible } ->
                Html.ul
                    [ class "paper-list" ]
                    (papers
                        |> Array.toList
                        |> List.filter (.title >> flip Set.member visible)
                        |> List.map (viewPaper model.titles model.authors model.links)
                    )
        ]


viewPaper :
    Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> Paper
    -> Html a
viewPaper titles authors links paper =
    Html.li
        [ class "paper" ]
        [ viewTitle titles links paper
        , viewDetails authors paper
        , viewEditLink paper
        ]


viewTitle : Dict TitleId Title -> Dict LinkId Link -> Paper -> Html a
viewTitle titles links paper =
    let
        title : String
        title =
            lookupTitle titles paper.title
    in
        Html.p [ class "title" ]
            [ case Array.get 0 paper.links of
                Nothing ->
                    Html.text title

                Just link ->
                    Html.a
                        [ class "link"
                        , href (lookupLink links link)
                        ]
                        [ Html.text title ]
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


viewDetails : Dict AuthorId Author -> Paper -> Html a
viewDetails authors paper =
    Html.p
        [ class "details" ]
        [ paper.authors
            |> Array.map (lookupAuthor authors)
            |> Array.toList
            |> String.join ", "
            |> Html.text
        , case paper.year of
            Nothing ->
                Html.text ""

            Just year ->
                Html.text (" [" ++ toString year ++ "]")
        ]



--------------------------------------------------------------------------------
-- Misc. utility functions


{-| Look up an author by id.
-}
lookupAuthor : Dict AuthorId Author -> AuthorId -> Author
lookupAuthor authors id =
    case Dict.get id authors of
        Nothing ->
            Debug.crash ("No author " ++ toString id)

        Just author ->
            author


{-| Look up a link by id.
-}
lookupLink : Dict LinkId Link -> LinkId -> Link
lookupLink links id =
    case Dict.get id links of
        Nothing ->
            Debug.crash ("No link " ++ toString id)

        Just link ->
            link


{-| Look up a title by id.
-}
lookupTitle : Dict TitleId Title -> TitleId -> Title
lookupTitle titles id =
    case Dict.get id titles of
        Nothing ->
            Debug.crash ("No title " ++ toString id)

        Just title ->
            title


arrayToDict : Array ( comparable, a ) -> Dict comparable a
arrayToDict =
    Array.foldl (uncurry Dict.insert) Dict.empty


{-| Collect the keys of a 'Dict' into a 'Set'
-}
dictKeysToSet : Dict comparable a -> Set comparable
dictKeysToSet =
    Dict.foldl (\k _ -> Set.insert k) Set.empty


{-| Dead-simple greedy fuzzy match algorithm. Search through the haystack for
the needle, in order, without backtracking.

      fuzzyMatch "XYZ" "..X..Y..Z.." = True

-}
fuzzyMatch : String -> String -> Bool
fuzzyMatch needle haystack =
    case String.uncons needle of
        Nothing ->
            True

        Just ( x, xs ) ->
            case String.indices (String.fromChar x) haystack of
                [] ->
                    False

                n :: _ ->
                    fuzzyMatch xs (String.dropLeft (n + 1) haystack)
