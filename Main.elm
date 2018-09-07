module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, padding, px, rgb, rgba, row, shrink, spaceEvenly, spacing)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (solid)
import Element.Font as Font exposing (color, family, sansSerif, typeface)
import Element.Input as Input exposing (text)
import Html exposing (Html, button, div, input, label, mark, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE exposing (splitAt)
import Regex exposing (Match, find, fromString)


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

        m =
            Regex.find toRegex model.thingToMatch
    in
    row
        [ Element.width (px 800), Element.height shrink, centerY, centerX, padding 10, spacing 10 ]
        [ el [ alignLeft, Element.width fill ]
            (Input.text [ Font.family [ Font.typeface "Consolas", Font.sansSerif ] ]
                { onChange = UpdateRegexStr
                , text = model.regexStr
                , placeholder = Just (Input.placeholder [] Element.none)
                , label = Input.labelAbove [] (Element.text "Regex")
                }
            )
        , el [ alignRight, Element.width fill ]
            (Input.text [ Font.family [ Font.typeface "Consolas", Font.sansSerif ], Background.color (rgba 255 255 255 0.1), Element.behindContent (el [] (divyUpMarks model.thingToMatch model.regexStr m)) ]
                { onChange = UpdateThingToMatch
                , text = model.thingToMatch
                , placeholder = Just (Input.placeholder [] Element.none)
                , label = Input.labelAbove [] (Element.text "Thing to match")
                }
            )
        ]


view : Model -> Html Msg
view model =
    Element.layout []
        (containerElement model)


checkIfInHead : Int -> List Int -> Bool
checkIfInHead d l =
    List.member d l


deepMember : Int -> List (List Int) -> Bool
deepMember d l =
    case l of
        [] ->
            False

        innerList ->
            case LE.uncons innerList of
                Just ( head, rest ) ->
                    if checkIfInHead d head then
                        True
                    else
                        deepMember d rest

                Nothing ->
                    False


divyUpMarks : String -> String -> List Match -> Element msg
divyUpMarks ogStr str m =
    let
        strAsList =
            ogStr
                |> String.toList
                |> List.map (\e -> String.fromChar e)

        listOfRanges =
            List.map
                (\e ->
                    let
                        length =
                            String.length e.match

                        range =
                            List.range e.index (e.index + length - 1)
                    in
                    range
                )
                m
    in
    row [ Font.family [ Font.typeface "Consolas", Font.sansSerif ], padding 10 ]
        (List.indexedMap
            (\i e ->
                let
                    isDeepMember =
                        deepMember i listOfRanges
                in
                if isDeepMember then
                    case e == " " of
                        True ->
                            let
                                _ =
                                    Debug.log "e" e
                            in
                            el
                                [ Font.color (Element.rgb 255 255 0)
                                , Element.width (px 11)
                                , Background.color (rgb 255 255 0)
                                ]
                                (Element.text "o")

                        False ->
                            el
                                [ Font.color (Element.rgb 255 255 0)
                                , Element.width fill
                                , Background.color (rgb 255 255 0)
                                ]
                                (Element.text e)
                else
                    case e == " " of
                        True ->
                            el
                                [ Font.color (Element.rgb 255 255 255)
                                , Element.width (px 11)
                                , Background.color (rgb 255 255 255)
                                , Border.solid
                                ]
                                (Element.text "o")

                        False ->
                            el
                                [ Font.color (Element.rgb 255 255 255)
                                , Element.width fill
                                , Background.color (rgb 255 255 255)
                                ]
                                (Element.text e)
            )
            strAsList
        )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
