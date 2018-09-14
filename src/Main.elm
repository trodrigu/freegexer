module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, padding, paddingEach, paddingXY, px, rgb, rgba, row, shrink, spaceEvenly, spacing, wrappedRow)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (solid)
import Element.Font as Font exposing (color, family, sansSerif, typeface)
import Element.Input as Input exposing (text)
import Html exposing (Html, button, div, input, label, mark, span, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE exposing (getAt, greedyGroupsOf, splitAt)
import Regex exposing (Match, find, fromString)
import String.Extra as SE exposing (softBreak)
import Utility exposing (checkIfInHead, deepMember)


type alias Model =
    { regexStr : String
    , thingToMatch : String
    }


initialModel : Model
initialModel =
    { regexStr = ""
    , thingToMatch = ""
    }


type Msg
    = UpdateRegexStr String
    | UpdateThingToMatch String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRegexStr str ->
            { model | regexStr = str }

        UpdateThingToMatch str ->
            { model | thingToMatch = str }


containerElement : Model -> Element Msg
containerElement model =
    let
        toRegex =
            model.regexStr
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        thingToMatchSoftBroke =
            softBreak 66 model.thingToMatch
                |> accomodateDoubleNewLines

        matches =
            List.map
                (\e ->
                    Regex.find toRegex e
                )
                thingToMatchSoftBroke
    in
    column
        [ Element.width (px 672)
        , Element.height shrink
        , centerY
        , centerX
        , padding 10
        ]
        [ el
            [ Element.width fill
            , padding 10
            ]
            (Input.text
                []
                { onChange = UpdateRegexStr
                , text = model.regexStr
                , placeholder = Just (Input.placeholder [] Element.none)
                , label = Input.labelAbove [] (Element.text "Regex")
                }
            )
        , el
            [ Element.width fill
            , padding 10
            ]
            (Input.multiline
                [ Background.color (rgba 255 255 255 0.1)
                , Element.behindContent (el [] (divyUpMarks model.thingToMatch model.regexStr matches))
                , Element.height (px 370)
                ]
                { onChange = UpdateThingToMatch
                , text = model.thingToMatch
                , placeholder = Just (Input.placeholder [] Element.none)
                , label = Input.labelAbove [] (Element.text "Thing to match")
                , spellcheck = False
                }
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family
            [ Font.external
                { name = "Fira Sans"
                , url = "https://fonts.googleapis.com/css?family=Fira+Sans"
                }
            ]
        ]
        (containerElement model)


listOfRanges : List (List Match) -> List (List (List Int))
listOfRanges m =
    List.map
        (\row ->
            List.map
                (\innerE ->
                    let
                        length =
                            String.length innerE.match

                        range =
                            List.range innerE.index (innerE.index + length - 1)
                    in
                    range
                )
                row
        )
        m



-- go through and see if it contains \n\n
-- if it goes through then add another row right after :)!


accomodateDoubleNewLines : List String -> List String
accomodateDoubleNewLines l =
    List.foldl
        (\e memo ->
            if String.contains "\n\n" e then
                let
                    splitString =
                        String.split "\n\n" e
                in
                memo ++ splitString
            else
                memo ++ [ e ]
        )
        []
        l


divyUpMarks : String -> String -> List (List Match) -> Element msg
divyUpMarks ogStr str matches =
    let
        strAsList =
            ogStr
                |> String.toList
                |> List.map (\e -> String.fromChar e)

        withResolvedWordBreaks =
            ogStr
                |> softBreak 66
                |> accomodateDoubleNewLines
                |> List.map
                    (\e -> String.split "" e)

        lRanges =
            listOfRanges matches
    in
    column [ padding 10 ]
        (List.indexedMap
            (\outerIndex innerRow ->
                let
                    insideRow =
                        innerRow
                in
                if innerRow == [] then
                    row
                        [ paddingXY 0 2
                        , Element.width fill
                        , Element.height shrink
                        ]
                        [ el
                            [ Font.color (Element.rgb 255 255 255)
                            , Background.color (rgb 255 255 255)
                            ]
                            (Element.text "a")
                        ]
                else
                    row [ paddingEach { top = 2, right = 0, bottom = 1, left = 0 }, Element.width fill, Element.height shrink ]
                        (List.indexedMap
                            (\i e ->
                                let
                                    matchRow =
                                        case getAt outerIndex lRanges of
                                            Just row ->
                                                row

                                            Nothing ->
                                                []

                                    isDeepMember =
                                        deepMember outerIndex i matchRow

                                    emptyStyle =
                                        case e of
                                            " " ->
                                                [ Element.htmlAttribute <| Attributes.style "display" "block" ]

                                            _ ->
                                                []

                                    style =
                                        if isDeepMember then
                                            [ Font.color (Element.rgb 255 255 0)
                                            , Background.color (rgb 255 255 0)
                                            ]
                                        else
                                            [ Font.color (Element.rgb 255 255 255)
                                            , Background.color (rgb 255 255 255)
                                            ]
                                in
                                el
                                    (style ++ emptyStyle)
                                    (Element.text e)
                            )
                            innerRow
                        )
            )
            withResolvedWordBreaks
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
