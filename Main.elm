module Main exposing (main)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, padding, px, rgb, rgba, row, shrink, spaceEvenly, spacing, wrappedRow)
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
            softBreak 84 model.thingToMatch

        m =
            List.map
                (\e ->
                    Regex.find toRegex e
                )
                thingToMatchSoftBroke
    in
    column
        [ Element.width fill, Element.height shrink, centerY, centerX, padding 10 ]
        [ el [ Element.width fill ]
            (Input.text [ Font.family [ Font.typeface "Consolas", Font.sansSerif ] ]
                { onChange = UpdateRegexStr
                , text = model.regexStr
                , placeholder = Just (Input.placeholder [] Element.none)
                , label = Input.labelAbove [] (Element.text "Regex")
                }
            )
        , el [ Element.width fill ]
            (Input.multiline
                [ Font.family
                    [ Font.typeface "Consolas"
                    , Font.sansSerif
                    ]
                , Background.color (rgba 255 255 255 0.1)
                , Element.behindContent (el [] (divyUpMarks model.thingToMatch model.regexStr m))
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
    Element.layout []
        (containerElement model)


checkIfInHead : Int -> List Int -> Bool
checkIfInHead d l =
    List.member d l


deepMember : Int -> Int -> List (List (List Int)) -> Bool
deepMember index d l =
    let
        matchRow =
            case getAt index l of
                Just row ->
                    row

                Nothing ->
                    []

        -- nothing =
        --     if index == 1 then
        --         let
        --             _ =
        --                 Debug.log "matchRow" matchRow
        --             _ =
        --                 Debug.log "d" d
        --         in
        --         0
        --     else
        --         0
    in
    case matchRow of
        [] ->
            False

        innerList ->
            let
                nothing =
                    if index == 1 then
                        let
                            _ =
                                Debug.log "innerList" innerList
                        in
                        0
                    else
                        0
            in
            case LE.uncons innerList of
                Just ( head, rest ) ->
                    if checkIfInHead d head then
                        True
                    else
                        -- let
                        --     nothing =
                        --         if index == 1 then
                        --             let
                        --                 _ =
                        --                     Debug.log "d" d
                        --                 _ =
                        --                     Debug.log "head" head
                        --             in
                        --             0
                        --         else
                        --             0
                        -- in
                        deepMember index d [ rest ]

                Nothing ->
                    False


resolveWordBreaks : List (List String) -> List (List String)
resolveWordBreaks list =
    let
        ( l, r ) =
            LE.mapAccuml
                (\memo e ->
                    let
                        reversedList =
                            e
                                |> List.reverse

                        updatedCurrent =
                            let
                                droppedList =
                                    LE.dropWhile
                                        (\dropE ->
                                            let
                                                result =
                                                    dropE /= " " && dropE /= "\n"
                                            in
                                            result
                                        )
                                        reversedList
                                        |> List.reverse
                            in
                            droppedList ++ memo

                        updatedMemo =
                            LE.takeWhile
                                (\takeE ->
                                    let
                                        result =
                                            takeE /= " " && takeE /= "\n"
                                    in
                                    result
                                )
                                reversedList
                                |> List.reverse
                    in
                    ( updatedMemo, updatedCurrent )
                )
                []
                list
    in
    r


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


divyUpMarks : String -> String -> List (List Match) -> Element msg
divyUpMarks ogStr str m =
    let
        strAsList =
            ogStr
                |> String.toList
                |> List.map (\e -> String.fromChar e)

        count =
            strAsList
                |> List.length

        listOfLists =
            strAsList
                |> greedyGroupsOf 84

        last =
            case LE.last listOfLists of
                Just innerl ->
                    innerl

                Nothing ->
                    []

        withResolvedWordBreaks =
            ogStr
                |> softBreak 84
                |> List.map
                    (\e -> String.split "" e)

        lRanges =
            listOfRanges m
    in
    column []
        (List.indexedMap
            (\outerIndex innerRow ->
                let
                    insideRow =
                        innerRow
                in
                row [ Element.width fill, Element.height shrink, padding 10 ]
                    (List.indexedMap
                        (\i e ->
                            let
                                isDeepMember =
                                    deepMember outerIndex i lRanges

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
