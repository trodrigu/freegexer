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
        adjustedWords =
            ogStr
                |> String.toList
                |> List.foldl
                    (\e memo ->
                        if e == '\n' then
                            memo
                                ++ [ "\n" ]
                        else
                            let
                                last =
                                    memo
                                        |> LE.last
                                        |> Maybe.withDefault ""

                                eAsString =
                                    e |> String.fromChar

                                updatedLast =
                                    last
                                        ++ eAsString

                                init =
                                    memo
                                        |> LE.init
                                        |> Maybe.withDefault []

                                updatedMemoLastString =
                                    init ++ [ updatedLast ]

                                updatedMemo =
                                    if last == "\n" then
                                        memo ++ [ eAsString ]
                                    else
                                        updatedMemoLastString
                            in
                            updatedMemo
                    )
                    []
                |> List.foldl
                    (\sentence splitSentenceMemo ->
                        let
                            splitSentence =
                                String.words sentence
                                    |> List.intersperse " "
                        in
                        if splitSentence == [ "" ] then
                            splitSentenceMemo ++ [ sentence ]
                        else
                            splitSentenceMemo
                                ++ splitSentence
                    )
                    []
                |> Debug.log "adjustedWords"
    in
    wrappedRow [ Element.width (px 718), paddingEach { bottom = 10, right = 47, left = 13, top = 13 } ]
        (List.indexedMap
            (\index word ->
                case word of
                    "\n" ->
                        let
                            newLineBeforeNewLine =
                                case index of
                                    0 ->
                                        False

                                    _ ->
                                        let
                                            prevIndex =
                                                index - 1

                                            prevElement =
                                                LE.getAt prevIndex adjustedWords
                                                    |> Maybe.withDefault ""
                                        in
                                        prevElement == "\n"
                        in
                        if newLineBeforeNewLine then
                            el [ Element.height (px 23), Element.width (fill |> Element.minimum 718) ] Element.none
                        else
                            el [ Element.width (fill |> Element.minimum 718) ] Element.none

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
                                gradient { angle = pi / 2, steps = colorList }
                        in
                        el [ paddingEach { top = 0, bottom = 3, right = 0, left = 0 }, updatedGradient ] (Element.text word)
            )
            adjustedWords
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
