module Main exposing (..)

import Array exposing (Array)
import ArrayExtra as Array
import BasicsExtra exposing (..)
import Dict exposing (Dict)
import DictExtra as Dict
import Http
import Html exposing (Html)
import HtmlExtra as Html
import Html.Attributes exposing (href, class)
import Html.Events
import HtmlEventsExtra as HtmlEvents
import Intersection exposing (Intersection)
import Json.Decode as Decode exposing (Decoder)
import JsonDecodeExtra as Decode
import ListExtra as List
import MaybeExtra as Maybe
import NoUiSlider exposing (..)
import Set exposing (Set)


--------------------------------------------------------------------------------
-- Types and type aliases


type alias Model =
    { papers : Array Paper
    , titles : Dict TitleId Title
    , authors : Dict AuthorId Author
    , links : Dict LinkId Link

    -- Cache the minimum and maximum years of all papers, with some weird
    -- unimportant default values since we assume that *some* paper has a year.
    , yearMin : Int
    , yearMax : Int

    -- An inverted index mapping author ids to the set of papers by that
    -- author.
    , authorsIndex : Dict AuthorId (Set TitleId)

    -- The author filter and a cached set of title ids that match the filter. We
    -- keep both the "live" filter (input text box) and every "facet" that has
    -- been snipped off by pressing 'enter'.
    , authorFilter : String
    , authorFilterIds : Intersection TitleId

    -- Invariant: non-empty strings
    , authorFacets : List ( String, Intersection TitleId )

    -- The year filter and a cached set of title ids that match the filter.
    -- When min == yearMin and max == yearMax + 1, we treat this as a "special"
    -- mode wherein we show papers without any year.
    , yearFilter : { min : Int, max : Int } -- (inclusive, exclusive)
    , yearFilterIds : Intersection TitleId
    }


type
    Message
    -- The initial download of ./static/papers.json
    = Blob (Result Http.Error Papers)
      -- The author filter input box contents were modified
    | AuthorFilter String
      -- 'Enter' was pressed in the author filter input box
    | AuthorFacetAdd
      -- An author was clicked, let's create a facet from it
    | AuthorFacetAdd_ Author
      -- An author facet was clicked, let's remove it
    | AuthorFacetRemove String
      -- The year filter slider was updated
    | YearFilter Int Int


type alias AuthorId =
    Int


{-| Author name. Invariant: non-empty.
-}
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



--------------------------------------------------------------------------------
-- Boilerplate main function


main : Program Never Model Message
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--------------------------------------------------------------------------------
-- Initialize the model and GET ./static/papers.json


init : ( Model, Cmd Message )
init =
    let
        decodePapers : Decoder Papers
        decodePapers =
            Decode.andThen3
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
            Decode.tuple2 Decode.int Decode.string
                |> Decode.array
                |> Decode.map Array.dict
    in
        ( { papers = Array.empty
          , titles = Dict.empty
          , authors = Dict.empty
          , links = Dict.empty
          , yearMin = 0
          , yearMax = 0
          , authorsIndex = Dict.empty
          , authorFilter = ""
          , authorFilterIds = Intersection.empty
          , authorFacets = []
          , yearFilter = { min = 0, max = 0 }
          , yearFilterIds = Intersection.empty
          }
        , Http.send Blob <| Http.get "./static/papers.json" decodePapers
        )


decodePaper :
    Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> Decoder Paper
decodePaper titles authors links =
    Decode.map7 Paper
        (Decode.intField "a")
        decodeAuthors
        (Decode.optIntField "c")
        decodeReferences
        decodeLinks
        (Decode.intField "f")
        (Decode.intField "g")


decodeAuthors : Decoder (Array AuthorId)
decodeAuthors =
    Decode.intArray
        |> Decode.field "b"
        |> Decode.withDefault Array.empty


decodeLinks : Decoder (Array LinkId)
decodeLinks =
    Decode.intArray
        |> Decode.field "e"
        |> Decode.withDefault Array.empty


decodeReferences : Decoder (Array TitleId)
decodeReferences =
    Decode.intArray
        |> Decode.field "d"
        |> Decode.withDefault Array.empty



--------------------------------------------------------------------------------
-- Subscriptions


subscriptions : Model -> Sub Message
subscriptions _ =
    let
        unpack : NoUiSliderOnUpdate -> Message
        unpack values =
            case values of
                [ n, m ] ->
                    YearFilter n m

                _ ->
                    Debug.crash <|
                        "Expected 2 ints; noUiSlider.js sent: "
                            ++ toString values
    in
        noUiSliderOnUpdate unpack



--------------------------------------------------------------------------------
-- The main update loop


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Blob blob ->
            handleBlob blob model

        AuthorFilter filter ->
            handleAuthorFilter filter model

        AuthorFacetAdd ->
            handleAuthorFacetAdd model

        AuthorFacetAdd_ author ->
            handleAuthorFacetAdd_ author model

        AuthorFacetRemove facet ->
            handleAuthorFacetRemove facet model

        YearFilter n m ->
            handleYearFilter n m model


handleBlob : Result Http.Error Papers -> Model -> ( Model, Cmd Message )
handleBlob result model =
    case result of
        Ok blob ->
            let
                ( yearMin, yearMax ) =
                    Array.foldl
                        (\paper ( n, m ) ->
                            case paper.year of
                                Nothing ->
                                    ( n, m )

                                Just y ->
                                    ( min n y, max m y )
                        )
                        ( 3000, 0 )
                        blob.papers

                model : Model
                model =
                    { papers = blob.papers
                    , titles = blob.titles
                    , authors = blob.authors
                    , links = blob.links
                    , yearMin = yearMin
                    , yearMax = yearMax
                    , authorsIndex = buildAuthorsIndex blob.papers blob.authors
                    , authorFilter = ""
                    , authorFilterIds = Intersection.empty
                    , authorFacets = []
                    , yearFilter = { min = yearMin, max = yearMax + 1 }
                    , yearFilterIds = Intersection.empty
                    }

                command : Cmd Message
                command =
                    noUiSliderCreate
                        { id = "year-slider"
                        , start = [ yearMin, yearMax + 1 ]
                        , margin = Just 1
                        , limit = Nothing
                        , connect = Just True
                        , direction = Nothing
                        , orientation = Nothing
                        , behavior = Nothing
                        , step = Just 1
                        , range = Just { min = yearMin, max = yearMax + 1 }
                        }
            in
                ( model, command )

        Err msg ->
            Debug.crash <| toString msg


handleAuthorFilter : String -> Model -> ( Model, Cmd a )
handleAuthorFilter s model =
    let
        authorFilter : String
        authorFilter =
            s

        authorFilterIds : Intersection TitleId
        authorFilterIds =
            buildAuthorFilterIds s model.authors model.authorsIndex

        model_ : Model
        model_ =
            { model
                | authorFilter = authorFilter
                , authorFilterIds = authorFilterIds
            }
    in
        ( model_, Cmd.none )


handleAuthorFacetAdd : Model -> ( Model, Cmd a )
handleAuthorFacetAdd model =
    let
        model_ : Model
        model_ =
            if String.isEmpty model.authorFilter then
                model
            else
                let
                    authorFilter : String
                    authorFilter =
                        ""

                    authorFacetIds : Intersection TitleId
                    authorFacetIds =
                        Intersection.empty

                    authorFacets : List ( String, Intersection TitleId )
                    authorFacets =
                        if List.member model.authorFilter (List.map Tuple.first model.authorFacets) then
                            model.authorFacets
                        else
                            ( model.authorFilter, model.authorFilterIds )
                                :: model.authorFacets
                in
                    { model
                        | authorFilter = authorFilter
                        , authorFilterIds = authorFacetIds
                        , authorFacets = authorFacets
                    }
    in
        ( model_, Cmd.none )


handleAuthorFacetAdd_ : Author -> Model -> ( Model, Cmd a )
handleAuthorFacetAdd_ author model =
    let
        authorFacets : List ( String, Intersection TitleId )
        authorFacets =
            if List.member author (List.map Tuple.first model.authorFacets) then
                model.authorFacets
            else
                let
                    authorFilterIds : Intersection TitleId
                    authorFilterIds =
                        buildAuthorFilterIds author model.authors model.authorsIndex
                in
                    ( author, authorFilterIds ) :: model.authorFacets

        model_ : Model
        model_ =
            { model | authorFacets = authorFacets }
    in
        ( model_, Cmd.none )


handleAuthorFacetRemove : String -> Model -> ( Model, Cmd a )
handleAuthorFacetRemove facet model =
    let
        authorFacets : List ( String, Intersection TitleId )
        authorFacets =
            model.authorFacets
                |> List.deleteBy (Tuple.first >> equals facet)

        model_ : Model
        model_ =
            { model
                | authorFacets = authorFacets
            }
    in
        ( model_, Cmd.none )


handleYearFilter : Int -> Int -> Model -> ( Model, Cmd a )
handleYearFilter n m model =
    let
        yearFilter : { min : Int, max : Int }
        yearFilter =
            { min = n, max = m }

        yearFilterIds : Intersection TitleId
        yearFilterIds =
            if n == model.yearMin && m == model.yearMax + 1 then
                Intersection.empty
            else
                model.papers
                    |> Array.foldl
                        (\paper ->
                            case paper.year of
                                Nothing ->
                                    identity

                                Just year ->
                                    if year >= n && year < m then
                                        Set.insert paper.title
                                    else
                                        identity
                        )
                        Set.empty
                    |> Intersection.fromSet

        model_ : Model
        model_ =
            { model
                | yearFilter = yearFilter
                , yearFilterIds = yearFilterIds
            }
    in
        ( model_, Cmd.none )


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


buildAuthorFilterIds :
    String
    -> Dict AuthorId Author
    -> Dict AuthorId (Set TitleId)
    -> Intersection TitleId
buildAuthorFilterIds s authors authorsIndex =
    if String.isEmpty s then
        Intersection.empty
    else
        authors
            |> Dict.foldl
                (\id author ->
                    if fuzzyMatch (String.toLower s) (String.toLower author) then
                        case Dict.get id authorsIndex of
                            Nothing ->
                                identity

                            Just ids ->
                                Set.union ids
                    else
                        identity
                )
                Set.empty
            |> Intersection.fromSet



--------------------------------------------------------------------------------
-- Render HTML


view : Model -> Html Message
view model =
    Html.div
        [ class "container" ]
        [ viewHeader model
        , viewPapers model
        ]


viewHeader : Model -> Html Message
viewHeader model =
    Html.header []
        [ Html.h1 [] [ Html.text "Haskell Papers" ]
        , Html.a
            [ class "subtle-link"
            , href "https://github.com/mitchellwrosen/haskell-papers"
            ]
            [ Html.text "contribute on GitHub" ]
        , viewAuthorSearchBox model.authorFilter
        , viewAuthorFacets <| List.map Tuple.first model.authorFacets
        , Html.div
            [ Html.Attributes.id "year-slider" ]
            []
        ]


viewAuthorSearchBox : String -> Html Message
viewAuthorSearchBox authorFilter =
    Html.div []
        [ Html.input
            [ class "search"
            , Html.Attributes.value authorFilter
            , Html.Attributes.placeholder "Search authors"
            , Html.Events.onInput AuthorFilter
            , HtmlEvents.onEnter AuthorFacetAdd
            ]
            []
        ]


viewAuthorFacets : List String -> Html Message
viewAuthorFacets authorFacets =
    case authorFacets of
        [] ->
            Html.empty

        facets ->
            facets
                |> List.map
                    (\facet ->
                        facet
                            |> Html.text
                            |> List.singleton
                            |> Html.div
                                [ class "facet"
                                , Html.Events.onClick (AuthorFacetRemove facet)
                                ]
                    )
                |> List.reverse
                |> Html.div [ class "facets" ]


viewPapers : Model -> Html Message
viewPapers model =
    Html.ul
        [ class "paper-list" ]
        (model.papers
            |> Array.toList
            |> List.map
                (viewPaper
                    (Intersection.toSet <|
                        List.foldl
                            (Tuple.second >> Intersection.append)
                            (Intersection.append model.authorFilterIds model.yearFilterIds)
                            model.authorFacets
                    )
                    model.titles
                    model.authors
                    model.links
                    model.authorFilter
                )
        )


viewPaper :
    Maybe (Set TitleId)
    -> Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> String
    -> Paper
    -> Html Message
viewPaper visible titles authors links authorFilter paper =
    Html.li
        (List.filterMap identity
            [ Just (class "paper")
            , case visible of
                Nothing ->
                    Nothing

                Just visible_ ->
                    if Set.member paper.title visible_ then
                        Nothing
                    else
                        Just (Html.Attributes.style [ ( "display", "none" ) ])
            ]
        )
        [ viewTitle titles links paper
        , viewDetails authors authorFilter paper
        , viewEditLink paper
        ]


viewTitle : Dict TitleId Title -> Dict LinkId Link -> Paper -> Html a
viewTitle titles links paper =
    let
        title : String
        title =
            Dict.unsafeGet titles paper.title
    in
        Html.p
            [ class "title" ]
            [ case Array.get 0 paper.links of
                Nothing ->
                    Html.text title

                Just link ->
                    Html.a
                        [ class "link"
                        , href (Dict.unsafeGet links link)
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


viewDetails : Dict AuthorId Author -> String -> Paper -> Html Message
viewDetails authors filter paper =
    Html.p
        [ class "details" ]
        [ paper.authors
            |> Array.map
                (\id ->
                    let
                        author : Author
                        author =
                            Dict.unsafeGet authors id
                    in
                        author
                            |> applyAuthorFilterStyle filter
                            |> Html.span
                                [ class "author"
                                , Html.Events.onClick <| AuthorFacetAdd_ author
                                ]
                )
            |> Array.toList
            |> Html.span []
        , case paper.year of
            Nothing ->
                Html.empty

            Just year ->
                Html.text (" [" ++ toString year ++ "]")
        ]


applyAuthorFilterStyle : String -> Author -> List (Html a)
applyAuthorFilterStyle filter author =
    case String.uncons filter of
        Nothing ->
            [ Html.text author ]

        Just ( x, xs ) ->
            case String.indices (String.toLower <| String.fromChar x) (String.toLower author) of
                [] ->
                    [ Html.text author ]

                n :: _ ->
                    Html.text (String.left n author)
                        :: Html.span
                            [ class "highlight" ]
                            [ Html.text (String.slice n (n + 1) author) ]
                        :: applyAuthorFilterStyle xs (String.dropLeft (n + 1) author)



--------------------------------------------------------------------------------
-- Misc. utility functions


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
