module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, minimum, padding, paddingEach, paddingXY, px, rgb, rgba, row, shrink, spaceEvenly, spacing, wrappedRow)
import Element.Background as Background exposing (color, gradient)
import Element.Border as Border exposing (solid)
import Element.Font as Font exposing (color, family, sansSerif, typeface)
import Element.Input as Input exposing (text)
import Html exposing (Html, button, div, hr, input, label, mark, span, text)
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

        matches =
            Regex.find toRegex model.thingToMatch
    in
    column
        [ Element.width (px 718)
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
                , Element.behindContent (el [] (divyUpMarks model.thingToMatch model.regexStr model))
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


listOfRanges : List Match -> List (List Int)
listOfRanges m =
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
        m


divyUpMarks : String -> String -> Model -> Element msg
divyUpMarks ogStr str model =
    let
        strAsList =
            ogStr
                |> String.toList

        wordsWithDoubleLineBreaks =
            ogStr
                |> String.split "\n\n"
    in
    case List.length wordsWithDoubleLineBreaks of
        1 ->
            let
                sentenceSplitAtNewLine =
                    wordsWithDoubleLineBreaks
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.split "\n"

                wordsSplitUpWithNewLine =
                    sentenceSplitAtNewLine
                        |> List.foldl
                            (\e memo ->
                                let
                                    innerWords =
                                        e
                                            |> String.words
                                            |> List.intersperse " "
                                in
                                memo ++ innerWords ++ [ "\n" ]
                            )
                            []
                        |> LE.init
                        |> Maybe.withDefault []
            in
            wrappedRow [ Element.width (px 718), paddingEach { bottom = 10, right = 47, left = 13, top = 13 } ]
                (List.indexedMap
                    (\index word ->
                        case word of
                            "\n" ->
                                el [ Element.width (fill |> minimum 718) ] Element.none

                            " " ->
                                el [ Element.htmlAttribute <| Attributes.style "display" "block" ] (Element.text word)

                            _ ->
                                let
                                    toRegex =
                                        model.regexStr
                                            |> Regex.fromString
                                            |> Maybe.withDefault Regex.never

                                    matchIndexes =
                                        Regex.find toRegex word
                                            |> listOfRanges

                                    wordAsList =
                                        word
                                            |> String.toList

                                    boolList =
                                        List.indexedMap (\i e -> deepMember i matchIndexes) wordAsList

                                    colorList =
                                        List.map
                                            (\e ->
                                                if e == True then
                                                    rgb 255 255 0
                                                else
                                                    rgb 255 255 255
                                            )
                                            boolList

                                    updatedGradient =
                                        gradient { angle = 0.0, steps = colorList }
                                in
                                el [ paddingEach { top = 0, bottom = 3, right = 0, left = 0 }, updatedGradient ] (Element.text word)
                    )
                    wordsSplitUpWithNewLine
                )

        _ ->
            let
                sentences =
                    wordsWithDoubleLineBreaks
                        |> List.foldl
                            (\e memo ->
                                let
                                    innerWordsWithDoubleLineBreaks =
                                        [ e ] ++ [ "\n\n" ]
                                in
                                memo ++ innerWordsWithDoubleLineBreaks
                            )
                            []
            in
            wrappedRow [ Element.width (px 718), paddingEach { bottom = 10, right = 47, left = 13, top = 13 } ]
                (List.foldl
                    (\sentence memo ->
                        case sentence of
                            "\n\n" ->
                                memo
                                    ++ [ el [ Element.width (fill |> minimum 718) ] Element.none
                                       , el [ Element.width (fill |> minimum 718), Element.height (px 23) ] Element.none
                                       ]

                            _ ->
                                let
                                    words =
                                        if String.contains "\n" sentence then
                                            let
                                                sentenceSplitAtNewLine =
                                                    sentence
                                                        |> String.split "\n"

                                                wordsSplitUpWithNewLine =
                                                    sentenceSplitAtNewLine
                                                        |> List.foldl
                                                            (\e wordMemo ->
                                                                let
                                                                    innerWords =
                                                                        e
                                                                            |> String.words
                                                                            |> List.intersperse " "
                                                                in
                                                                wordMemo ++ innerWords ++ [ "\n" ]
                                                            )
                                                            []
                                                        |> LE.init
                                                        |> Maybe.withDefault []
                                            in
                                            wordsSplitUpWithNewLine
                                        else
                                            sentence
                                                |> String.words
                                                |> List.intersperse " "

                                    elements =
                                        List.foldl
                                            (\word innerMemo ->
                                                case word of
                                                    "\n" ->
                                                        innerMemo ++ [ el [ Element.width (fill |> minimum 718) ] Element.none ]

                                                    " " ->
                                                        innerMemo ++ [ el [ Element.htmlAttribute <| Attributes.style "display" "block" ] (Element.text word) ]

                                                    _ ->
                                                        let
                                                            toRegex =
                                                                model.regexStr
                                                                    |> Regex.fromString
                                                                    |> Maybe.withDefault Regex.never

                                                            matchIndexes =
                                                                Regex.find toRegex word
                                                                    |> listOfRanges

                                                            wordAsList =
                                                                word
                                                                    |> String.toList

                                                            boolList =
                                                                List.indexedMap (\i boolE -> deepMember i matchIndexes) wordAsList

                                                            colorList =
                                                                List.map
                                                                    (\colorE ->
                                                                        if colorE == True then
                                                                            rgb 255 255 0
                                                                        else
                                                                            rgb 255 255 255
                                                                    )
                                                                    boolList

                                                            updatedGradient =
                                                                gradient { angle = 0.0, steps = colorList }
                                                        in
                                                        innerMemo ++ [ el [ paddingEach { top = 0, bottom = 3, right = 0, left = 0 }, updatedGradient ] (Element.text word) ]
                                            )
                                            []
                                            words
                                in
                                memo ++ elements
                    )
                    []
                    sentences
                )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
