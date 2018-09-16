module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, padding, paddingEach, paddingXY, px, rgb, rgba, row, shrink, spaceEvenly, spacing, wrappedRow)
import Element.Background as Background exposing (color, gradient)
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

        matches =
            Regex.find toRegex model.thingToMatch
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
                , Element.behindContent (el [] (divyUpMarks model.thingToMatch model.regexStr model))
                , Element.height (px 370)
                , Element.htmlAttribute <| Attributes.style "wrap" "off"
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

        words =
            ogStr
                |> Debug.log "ogStr"
                |> String.words
                |> List.intersperse " "
                |> Debug.log "words"
    in
    wrappedRow [ paddingEach { bottom = 10, right = 10, left = 13, top = 13 } ]
        (List.indexedMap
            (\index word ->
                -- let
                -- isDeepMember =
                --     deepMember index lRanges
                -- emptyStyle =
                --     case character of
                --         " " ->
                --             [ Element.htmlAttribute <| Attributes.style "display" "block" ]
                --         _ ->
                --             []
                -- style =
                --     if isDeepMember then
                --         [ Font.color (Element.rgb 255 255 0)
                --         , Background.color (rgb 255 255 0)
                --         ]
                --     else
                --         [ Font.color (Element.rgb 255 255 255)
                --         , Background.color (rgb 255 255 255)
                --         ]
                -- create a new a el on first
                -- keep adding to text until \s
                -- when \s then cap it off and then
                -- use gradient after finding percentage (1/elements)
                -- in
                case word == " " of
                    -- for a given word determine gradients!
                    -- check all of the indexes
                    True ->
                        el [ Element.htmlAttribute <| Attributes.style "display" "block" ] (Element.text word)

                    False ->
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
             -- if index == 0 of
             --     el
             --         (style ++ emptyStyle)
             --         (Element.text character)
             -- else
            )
            words
         --  el [] (text "hello") el [] (text "world")
         --  make el match up exactly first then do the gradient hack
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
