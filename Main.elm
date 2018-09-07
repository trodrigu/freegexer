module Main exposing (main)

import Browser
import Element exposing (Element, column, el, padding, px, rgb, row, spacing)
import Element.Background as Background exposing (color)
import Element.Input as Input exposing (text)
import Html exposing (Html, button, div, input, label, mark, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse as EE exposing (onClick)
import List.Extra as LE exposing (splitAt)
import Regex exposing (Match, find, fromString)


type alias Model =
    { regexStr : String
    , thingToMatch : String
    , cursor : ( Float, Float )
    }


initialModel : Model
initialModel =
    { regexStr = ""
    , thingToMatch = ""
    , cursor = ( 0.0, 0.0 )
    }


type Msg
    = UpdateRegexStr String
    | UpdateThingToMatch String
    | MoveMsg ( Float, Float )


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRegexStr str ->
            { model | regexStr = str }

        UpdateThingToMatch str ->
            { model | thingToMatch = str }

        MoveMsg ( x, y ) ->
            { model | cursor = ( x, y ) }


containerElement : Model -> Element Msg
containerElement model =
    let
        ( l, r ) =
            model.cursor

        coordinatesAsString =
            "( " ++ (l |> String.fromFloat) ++ ", " ++ (r |> String.fromFloat) ++ " )"

        toRegex =
            model.regexStr
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        m =
            Regex.find toRegex model.thingToMatch

        cursorLine =
            let
                rAsFloat =
                    floor r
            in
            if rAsFloat > 70 then
                2
            else
                1

        cursor =
            el [] (Element.text "|")

        renderLines =
            if cursorLine == 1 then
                el [ Background.color (rgb 214 214 214), padding 10 ]
                    (column []
                        [ row [ Element.height (px 25) ]
                            [ cursor ]
                        , row [ Element.height (px 25) ]
                            []
                        ]
                    )
            else
                el [ Background.color (rgb 214 214 214), padding 10 ]
                    (column []
                        [ row [ Element.height (px 25) ]
                            []
                        , row []
                            [ column [ Element.height (px 25) ]
                                [ cursor ]
                            ]
                        ]
                    )
    in
    el []
        (column
            []
            [ row
                [ padding 10 ]
                [ Input.text []
                    { onChange = UpdateRegexStr
                    , text = model.regexStr
                    , placeholder = Just (Input.placeholder [] (Element.text "\\w"))
                    , label = Input.labelAbove [] (Element.text "Regex")
                    }
                ]
            , row
                [ padding 10 ]
                [ Input.text []
                    { onChange = UpdateThingToMatch
                    , text = model.thingToMatch
                    , placeholder = Just (Input.placeholder [] (Element.text "hello world"))
                    , label = Input.labelAbove [] (Element.text "Thing to match")
                    }
                ]
            , divyUpMarks model.thingToMatch model.regexStr m
            , renderLines
            , row [] [ Element.paragraph [] [ Element.text coordinatesAsString ] ]
            , row [] [ Element.paragraph [] [ Element.text (cursorLine |> String.fromInt) ] ]
            ]
        )


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
    row [ padding 10 ]
        (List.indexedMap
            (\i e ->
                let
                    isDeepMember =
                        deepMember i listOfRanges
                in
                if isDeepMember then
                    el [ Background.color (rgb 255 255 0) ] (Element.text e)
                else
                    case e of
                        " " ->
                            el [ padding 5 ] (Element.text " ")

                        _ ->
                            el [] (Element.text e)
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
