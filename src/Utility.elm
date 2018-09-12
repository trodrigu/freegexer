module Utility exposing (checkIfInHead, deepMember)

import List.Extra as LE exposing (getAt, greedyGroupsOf, splitAt)


deepMember : Int -> Int -> List (List Int) -> Bool
deepMember index d l =
    case l of
        [] ->
            False

        innerList ->
            case LE.uncons innerList of
                Just ( head, rest ) ->
                    if checkIfInHead d head then
                        True
                    else
                        deepMember index d rest

                Nothing ->
                    False


checkIfInHead : Int -> List Int -> Bool
checkIfInHead d l =
    List.member d l
