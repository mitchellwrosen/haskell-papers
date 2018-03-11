module Main exposing (..)

import Array exposing (Array)
import ArrayExtra as Array
import BasicsExtra exposing (..)
import Dict exposing (Dict)
import DictExtra as Dict
import Http
import Html exposing (Html)
import Html.Attributes exposing (href, class)
import Html.Events
import Intersection exposing (Intersection)
import Json.Decode as Decode exposing (Decoder)
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
    , authorFacets : List ( String, Set TitleId ) -- Invariant: non-empty strings

    -- The year filter and a cached set of title ids that match the filter.
    -- When min == yearMin and max == yearMax + 1, we tread this as a "special"
    -- mode wherein we show papers without any year.
    , yearFilter : { min : Int, max : Int } -- (inclusive, exclusive)
    , yearFilterIds : Intersection TitleId
    }


type Message
    = Blob (Result Http.Error Papers)
    | AuthorFilter String
    | AuthorFacetAdd -- 'Enter' pressed in author filter input box
    | AuthorFacetRemove String
    | YearFilter Int Int


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
            Decode.map Array.dict <|
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
            in
                ( { papers = blob.papers
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
                , noUiSliderCreate
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
                )

        Err msg ->
            Debug.crash <| toString msg


handleAuthorFilter : String -> Model -> ( Model, Cmd a )
handleAuthorFilter s model =
    let
        authorFilterIds : Intersection TitleId
        authorFilterIds =
            if String.isEmpty s then
                Intersection.empty
            else
                model.authors
                    |> Dict.foldl
                        (\id author ->
                            if fuzzyMatch (String.toLower s) (String.toLower author) then
                                Set.insert id
                            else
                                identity
                        )
                        Set.empty
                    |> Set.foldl
                        (\id ->
                            case Dict.get id model.authorsIndex of
                                Nothing ->
                                    identity

                                Just ids ->
                                    Set.union ids
                        )
                        Set.empty
                    |> Intersection.fromSet
    in
        ( { model
            | authorFilter = s
            , authorFilterIds = authorFilterIds
          }
        , Cmd.none
        )


handleAuthorFacetAdd : Model -> ( Model, Cmd a )
handleAuthorFacetAdd model =
    let
        model_ : Model
        model_ =
            if String.isEmpty model.authorFilter then
                model
            else
                { model
                    | authorFilter = ""
                    , authorFilterIds = Intersection.empty
                    , authorFacets =
                        if List.member model.authorFilter (List.map Tuple.first model.authorFacets) then
                            model.authorFacets
                        else
                            ( model.authorFilter, Intersection.unsafeToSet model.authorFilterIds )
                                :: model.authorFacets
                }
    in
        ( model_, Cmd.none )


handleAuthorFacetRemove : String -> Model -> ( Model, Cmd a )
handleAuthorFacetRemove facet model =
    let
        authorFacets : List ( String, Set TitleId )
        authorFacets =
            model.authorFacets
                |> List.deleteBy (Tuple.first >> equals facet)
    in
        ( { model
            | authorFacets = authorFacets
          }
        , Cmd.none
        )


handleYearFilter : Int -> Int -> Model -> ( Model, Cmd a )
handleYearFilter n m model =
    ( { model
        | yearFilter = { min = n, max = m }
        , yearFilterIds =
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
      }
    , Cmd.none
    )


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



--------------------------------------------------------------------------------
-- Render HTML


view : Model -> Html Message
view model =
    Html.div
        [ class "container" ]
        [ Html.header []
            [ Html.h1 [] [ Html.text "Haskell Papers" ]
            , Html.a
                [ class "subtle-link"
                , href "https://github.com/mitchellwrosen/haskell-papers"
                ]
                [ Html.text "contribute on GitHub" ]
            , Html.div []
                [ Html.input
                    [ Html.Attributes.value model.authorFilter
                    , Html.Events.onInput AuthorFilter
                    , Html.Events.on "keyup"
                        (Html.Events.keyCode
                            |> Decode.andThen
                                (\code ->
                                    if
                                        -- enter
                                        code == 13
                                    then
                                        Decode.succeed AuthorFacetAdd
                                    else
                                        Decode.fail ""
                                )
                        )
                    ]
                    []
                ]
            , case model.authorFacets of
                [] ->
                    Html.text ""

                facets ->
                    facets
                        |> List.map
                            (\( facet, _ ) ->
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
            , Html.div
                [ Html.Attributes.id "year-slider" ]
                []
            ]
        , case model of
            { papers, titles, authorFacets, authorFilterIds, yearFilterIds } ->
                Html.ul
                    [ class "paper-list" ]
                    (papers
                        |> Array.toList
                        |> List.map
                            (viewPaper
                                (Intersection.toSet <|
                                    List.foldl
                                        (Tuple.second >> Intersection.fromSet >> Intersection.append)
                                        (Intersection.append authorFilterIds yearFilterIds)
                                        authorFacets
                                )
                                model.titles
                                model.authors
                                model.links
                                model.authorFilter
                            )
                    )
        ]


viewPaper :
    Maybe (Set TitleId)
    -> Dict TitleId Title
    -> Dict AuthorId Author
    -> Dict LinkId Link
    -> String
    -> Paper
    -> Html a
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


viewDetails : Dict AuthorId Author -> String -> Paper -> Html a
viewDetails authors filter paper =
    Html.p
        [ class "details" ]
        [ paper.authors
            |> Array.map
                (Dict.unsafeGet authors
                    >> applyAuthorFilterStyle filter
                    >> Html.span []
                )
            |> Array.toList
            |> List.intersperse (Html.text ", ")
            |> Html.span []
        , case paper.year of
            Nothing ->
                Html.text ""

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
