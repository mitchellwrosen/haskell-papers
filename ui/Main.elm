module Main exposing (..)

import Array exposing (Array)
import ArrayExtra as Array
import BasicsExtra exposing (..)
import Date exposing (Date)
import Dict exposing (Dict)
import DictExtra as Dict
import Either exposing (Either(..), either)
import Http
import Html exposing (Html)
import HtmlExtra as Html
import Html.Attributes exposing (href, class)
import Html.Events
import Html.Lazy as Html
import HtmlEventsExtra as HtmlEvents
import Intersection exposing (Intersection)
import Intervals exposing (Intervals)
import Json.Decode as Decode exposing (Decoder)
import JsonDecodeExtra as Decode
import Lazy exposing (Lazy, force, lazy)
import ListExtra as List
import MaybeExtra as Maybe
import NoUiSlider exposing (..)
import Set exposing (Set)
import Setter exposing (..)
import String exposing (toLower)
import StringExtra as String exposing (words_)
import Task
import TaskExtra as Task


--------------------------------------------------------------------------------
-- Types and type aliases


type TheModel
    = Loading
    | Loaded Model


type alias Model =
    { -- The current date
      now : Date

    -- Basic paper structures
    , papers : Array Paper
    , authors : Dict AuthorId (Lazy Author)

    -- Cached min/max years
    , yearMin : Int
    , yearMax : Int

    -- The title filter and a cached set of title ids that match the filter.
    , titleFilter : String
    , titleFilterIds : Intersection TitleId

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

    -- The intersection of 'titleFilterIds', 'authorFilterIds', 'authorFacets',
    -- and 'yearFilterIds'.
    , visibleIds : Intersection TitleId
    }


type
    Message
    -- The initial download of ./static/papers.json and the current date.
    = Blob (Result Http.Error ( Papers, Date ))
      -- The title filter input box contents were modified
    | TitleFilter String
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
    { titles : Dict TitleId (Lazy Title)
    , authors : Dict AuthorId (Lazy Author)
    , links : Dict LinkId (Lazy Link)
    , authorsIndex : Dict AuthorId (Set TitleId)
    , yearMin : Int
    , yearMax : Int
    , papers : Array Paper
    }


type alias Paper =
    { titleId : TitleId
    , title : Lazy Title
    , authors : Array (Lazy Author)
    , year : Maybe Int
    , references : Array TitleId -- FIXME: Array (Lazy Title)
    , citations : Array TitleId -- FIXME: Array (Lazy Title)
    , links : Array (Lazy Link)
    , loc : { file : Int, line : Int }
    }



--------------------------------------------------------------------------------
-- Boilerplate main function


main : Program Never TheModel Message
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--------------------------------------------------------------------------------
-- Initialize the model and GET ./static/papers.json


init : ( TheModel, Cmd Message )
init =
    let
        decodePapers : Decoder Papers
        decodePapers =
            Decode.field "a" decodeIds_
                |> Decode.andThen
                    (\titles ->
                        Decode.field "b" decodeIds_
                            |> Decode.andThen
                                (\authors ->
                                    Decode.field "c" decodeIds_
                                        |> Decode.andThen
                                            (\links ->
                                                Decode.succeed Papers
                                                    |> Decode.ap (Decode.succeed titles)
                                                    |> Decode.ap (Decode.succeed authors)
                                                    |> Decode.ap (Decode.succeed links)
                                                    |> Decode.ap (Decode.field "e" decodeIndex)
                                                    |> Decode.ap (Decode.intField "f")
                                                    |> Decode.ap (Decode.intField "g")
                                                    |> Decode.ap
                                                        (decodePaper titles authors links
                                                            |> Decode.array
                                                            |> Decode.field "d"
                                                        )
                                            )
                                )
                    )

        decodeIndex : Decoder (Dict Int (Set Int))
        decodeIndex =
            Decode.tuple2 Decode.int Decode.intSet
                |> Decode.array
                |> Decode.map Array.toDict

        decodeIds : Decoder (Dict Int String)
        decodeIds =
            Decode.tuple2 Decode.int Decode.string
                |> Decode.array
                |> Decode.map Array.toDict

        decodeIds_ : Decoder (Dict Int (Lazy String))
        decodeIds_ =
            Decode.tuple2 Decode.int (Decode.map (always >> lazy) Decode.string)
                |> Decode.array
                |> Decode.map Array.toDict
    in
        ( Loading
        , Http.get "./static/papers.json" decodePapers
            |> Http.toTask
            |> Task.and Date.now
            |> Task.attempt Blob
        )


decodePaper :
    Dict TitleId (Lazy Title)
    -> Dict AuthorId (Lazy Author)
    -> Dict LinkId (Lazy Link)
    -> Decoder Paper
decodePaper titles authors links =
    Decode.intField "a"
        |> Decode.andThen
            (\titleId ->
                decodeAuthors
                    |> Decode.andThen
                        (\authorIds ->
                            decodeLinks
                                |> Decode.andThen
                                    (\linkIds ->
                                        Decode.succeed Paper
                                            |> Decode.ap (Decode.succeed titleId)
                                            |> Decode.ap (Decode.succeed (Dict.unsafeGet titles titleId))
                                            |> Decode.ap (Decode.succeed (Array.map (Dict.unsafeGet authors) authorIds))
                                            |> Decode.ap (Decode.optIntField "c")
                                            |> Decode.ap decodeReferences
                                            |> Decode.ap decodeCitations
                                            |> Decode.ap (Decode.succeed (Array.map (Dict.unsafeGet links) linkIds))
                                            |> Decode.ap
                                                (Decode.map2
                                                    (\file line -> { file = file, line = line })
                                                    (Decode.intField "f")
                                                    (Decode.intField "g")
                                                )
                                    )
                        )
            )


decodeAuthors : Decoder (Array AuthorId)
decodeAuthors =
    Decode.intArray
        |> Decode.field "b"
        |> Decode.withDefault Array.empty


decodeCitations : Decoder (Array TitleId)
decodeCitations =
    Decode.intArray
        |> Decode.field "h"
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


subscriptions : TheModel -> Sub Message
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Loaded _ ->
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


update : Message -> TheModel -> ( TheModel, Cmd Message )
update message model =
    case ( message, model ) of
        ( Blob blob, Loading ) ->
            handleBlob blob

        ( TitleFilter filter, Loaded model ) ->
            handleTitleFilter filter model

        ( AuthorFilter filter, Loaded model ) ->
            handleAuthorFilter filter model

        ( AuthorFacetAdd, Loaded model ) ->
            handleAuthorFacetAdd model

        ( AuthorFacetAdd_ author, Loaded model ) ->
            handleAuthorFacetAdd_ author model

        ( AuthorFacetRemove facet, Loaded model ) ->
            handleAuthorFacetRemove facet model

        ( YearFilter n m, Loaded model ) ->
            handleYearFilter n m model

        _ ->
            ( model, Cmd.none )
                |> Debug.log ("Ignoring message: " ++ toString message)


handleBlob : Result Http.Error ( Papers, Date ) -> ( TheModel, Cmd Message )
handleBlob result =
    case result of
        Ok ( blob, now ) ->
            let
                model : Model
                model =
                    { now = now
                    , papers = blob.papers
                    , authors = blob.authors
                    , yearMin = blob.yearMin
                    , yearMax = blob.yearMax
                    , titleFilter = ""
                    , titleFilterIds = Intersection.empty
                    , authorsIndex = blob.authorsIndex
                    , authorFilter = ""
                    , authorFilterIds = Intersection.empty
                    , authorFacets = []
                    , yearFilter =
                        { min = blob.yearMin
                        , max = blob.yearMax + 1
                        }
                    , yearFilterIds = Intersection.empty
                    , visibleIds = Intersection.empty
                    }

                command : Cmd Message
                command =
                    noUiSliderCreate
                        { id = "year-slider"
                        , start = [ blob.yearMin, blob.yearMax + 1 ]
                        , margin = Just 1
                        , limit = Nothing
                        , connect = Just True
                        , direction = Nothing
                        , orientation = Nothing
                        , behavior = Nothing
                        , step = Just 1
                        , range =
                            Just
                                { min = blob.yearMin
                                , max = blob.yearMax + 1
                                }
                        }
            in
                ( Loaded model, command )

        Err msg ->
            Debug.crash <| toString msg


handleTitleFilter : String -> Model -> ( TheModel, Cmd a )
handleTitleFilter filter model =
    let
        matches : Title -> Bool
        matches =
            toLower
                >> (filter
                        |> words_
                        |> List.map toLower
                        |> String.containsAll
                   )

        titleFilterIds : Intersection TitleId
        titleFilterIds =
            model.papers
                |> Array.foldl
                    (\paper ->
                        if matches (force paper.title) then
                            Set.insert paper.titleId
                        else
                            identity
                    )
                    Set.empty
                |> Intersection.fromSet

        model_ : Model
        model_ =
            { model
                | titleFilter = filter
                , titleFilterIds = titleFilterIds
            }
                |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


handleAuthorFilter : String -> Model -> ( TheModel, Cmd a )
handleAuthorFilter filter model =
    let
        authorFilterIds : Intersection TitleId
        authorFilterIds =
            buildAuthorFilterIds filter model.authors model.authorsIndex

        model_ : Model
        model_ =
            { model
                | authorFilter = filter
                , authorFilterIds = authorFilterIds
            }
                |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


handleAuthorFacetAdd : Model -> ( TheModel, Cmd a )
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
                        |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


handleAuthorFacetAdd_ : Author -> Model -> ( TheModel, Cmd a )
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
                |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


handleAuthorFacetRemove : String -> Model -> ( TheModel, Cmd a )
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
                |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


handleYearFilter : Int -> Int -> Model -> ( TheModel, Cmd a )
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
                                        Set.insert paper.titleId
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
                |> rebuildVisibleIds
    in
        ( Loaded model_, Cmd.none )


buildAuthorFilterIds :
    String
    -> Dict AuthorId (Lazy Author)
    -> Dict AuthorId (Set TitleId)
    -> Intersection TitleId
buildAuthorFilterIds filter authors authorsIndex =
    if String.isEmpty filter then
        Intersection.empty
    else
        let
            matches : Author -> Bool
            matches =
                toLower
                    >> (filter
                            |> words_
                            |> List.map toLower
                            |> String.containsAll
                       )
        in
            authors
                |> Dict.foldl
                    (\id author ->
                        if matches (force author) then
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


rebuildVisibleIds : Model -> Model
rebuildVisibleIds model =
    let
        visibleIds : Intersection TitleId
        visibleIds =
            List.foldl
                (Tuple.second >> Intersection.append)
                Intersection.empty
                model.authorFacets
                |> Intersection.append model.titleFilterIds
                |> Intersection.append model.authorFilterIds
                |> Intersection.append model.yearFilterIds
    in
        { model | visibleIds = visibleIds }



--------------------------------------------------------------------------------
-- Render HTML


view : TheModel -> Html Message
view model =
    case model of
        Loading ->
            Html.div
                [ class "container" ]
                [ Html.header [] [ Html.h2 [] [ Html.text "Rendering..." ] ] ]

        Loaded model ->
            Html.div
                [ class "container" ]
                [ viewHeader (Array.length model.papers)
                , viewPaperOfTheDay model
                , viewFilters model
                , viewPapers model
                ]


viewHeader : Int -> Html a
viewHeader n =
    Html.header []
        [ Html.h1 [] [ Html.text <| toString n ++ " Haskell Papers" ]
        , Html.thunk
            (Html.a
                [ class "subtle-link"
                , href "https://github.com/mitchellwrosen/haskell-papers"
                ]
                [ Html.div [] [ Html.text "contribute on GitHub" ] ]
            )
        ]


viewFilters : Model -> Html Message
viewFilters model =
    Html.p []
        [ Html.lazy viewTitleSearchBox model.titleFilter
        , Html.lazy viewAuthorSearchBox model.authorFilter
        , Html.lazy viewAuthorFacets <| List.map Tuple.first model.authorFacets
        , Html.thunk
            (Html.div
                [ Html.Attributes.id "year-slider" ]
                []
            )
        ]


viewTitleSearchBox : String -> Html Message
viewTitleSearchBox filter =
    Html.div
        [ class "title-search" ]
        [ Html.input
            [ class "title-search-box"
            , Html.Attributes.value filter
            , Html.Attributes.placeholder "Search titles"
            , Html.Events.onInput TitleFilter
            ]
            []
        ]


viewAuthorSearchBox : String -> Html Message
viewAuthorSearchBox filter =
    Html.div
        [ class "author-search" ]
        [ Html.input
            [ class "author-search-box"
            , Html.Attributes.value filter
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


viewPaperOfTheDay : Model -> Html Message
viewPaperOfTheDay model =
    case Array.getCycle (Date.day model.now) model.papers of
        Nothing ->
            Html.empty

        Just paper ->
            Html.div []
                [ Html.h3 [] [ Html.text "Paper of the Day" ]
                , Html.div [ class "paper" ]
                    -- FIXME(mitchell): Share code with 'viewPaper' below
                    [ Html.lazy
                        (viewTitle
                            (force paper.title)
                            (Array.get 0 paper.links)
                        )
                        Nothing
                    , Html.p
                        [ class "details" ]
                        [ Html.lazy
                            (viewAuthors paper.authors)
                            Nothing
                        , Html.lazy viewYear paper.year
                        , Html.lazy viewCitations paper.citations
                        ]
                    , Html.lazy viewEditLink paper.loc
                    ]
                ]


viewPapers : Model -> Html Message
viewPapers model =
    Html.ul
        [ class "paper-list" ]
        (model.papers
            |> Array.toList
            |> List.map (viewPaper model)
        )


viewPaper : Model -> Paper -> Html Message
viewPaper model paper =
    Html.li
        (List.filterMap identity
            [ Just (class "paper")
            , if Intersection.member paper.titleId model.visibleIds then
                Nothing
              else
                Just (Html.Attributes.style [ ( "display", "none" ) ])
            ]
        )
        [ Html.lazy
            (viewTitle
                (force paper.title)
                (Array.get 0 paper.links)
            )
            (Just model.titleFilter)
        , Html.p
            [ class "details" ]
            [ Html.lazy
                (viewAuthors paper.authors)
                (Just model.authorFilter)
            , Html.lazy viewYear paper.year
            , Html.lazy viewCitations paper.citations
            ]
        , Html.lazy viewEditLink paper.loc
        ]


viewTitle :
    Title -- Paper title
    -> Maybe (Lazy Link) -- Paper link
    -> Maybe String -- Title filter, or Nothing to ignore filter
    -> Html a
viewTitle title link filter =
    let
        title_ : List (Html a)
        title_ =
            case filter of
                Nothing ->
                    [ Html.text title ]

                Just filter ->
                    applyLiveFilterStyle (words_ filter) title
    in
        Html.p
            [ class "title" ]
            (case link of
                Nothing ->
                    title_

                Just link ->
                    [ Html.a
                        [ class "link"
                        , href (force link)
                        ]
                        title_
                    ]
            )


viewEditLink : { file : Int, line : Int } -> Html a
viewEditLink { file, line } =
    let
        editLink : String
        editLink =
            "https://github.com/mitchellwrosen/haskell-papers/edit/master/papers"
                ++ String.padLeft 3 '0' (toString file)
                ++ ".yaml#L"
                ++ toString line
    in
        Html.a
            [ class "subtle-link edit", href editLink ]
            [ Html.text "(edit)" ]


viewAuthors : Array (Lazy Author) -> Maybe String -> Html Message
viewAuthors authors filter =
    authors
        |> Array.map
            (\author ->
                author
                    |> maybe
                        (force >> Html.text >> List.singleton)
                        (\filter -> force >> applyLiveFilterStyle (words_ filter))
                        filter
                    |> Html.span
                        [ class "author"
                        , Html.Events.onClick <| AuthorFacetAdd_ (force author)
                        ]
            )
        |> Array.toList
        |> Html.span []


viewYear : Maybe Int -> Html a
viewYear year =
    case year of
        Nothing ->
            Html.empty

        Just year ->
            Html.text (" [" ++ toString year ++ "]")


viewCitations : Array TitleId -> Html a
viewCitations citations =
    case Array.length citations of
        0 ->
            Html.empty

        n ->
            Html.text (" (cited by " ++ toString n ++ ")")


applyLiveFilterStyle : List String -> String -> List (Html a)
applyLiveFilterStyle needles haystack =
    let
        segments : List (Either String String)
        segments =
            needles
                |> List.filterMap (String.interval >> apply haystack)
                |> List.foldl Intervals.insert Intervals.empty
                |> String.explode
                |> apply haystack

        renderSegment : Either String String -> Html a
        renderSegment =
            either
                Html.text
                (Html.text >> List.singleton >> Html.span [ class "highlight" ])
    in
        List.map renderSegment segments
