module UtilityTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (Test, describe, test)
import Utility exposing (deepMember)


exampleRangeList : List (List Int)
exampleRangeList =
    [ [ 1 ]
    , [ 2 ]
    ]


exampleNonMatchingRangeList : List (List Int)
exampleNonMatchingRangeList =
    [ [ 1 ]
    ]


bugRangeList : List (List Int)
bugRangeList =
    [ [ 0 ]
    , [ 1 ]
    ]


exampleOuterIndex : Int
exampleOuterIndex =
    1


exampleLetterIndex : Int
exampleLetterIndex =
    1


suite : Test
suite =
    describe "deepMember"
        [ test "returns True for match on 2nd row" <|
            \_ ->
                Expect.true "The deepMember is True for second row match" (deepMember 1 exampleLetterIndex exampleRangeList)
        , test "returns True for match on 1st row" <|
            \_ ->
                Expect.true "The deepMember is True for first row match" (deepMember 0 exampleLetterIndex exampleRangeList)
        , test "no match on 1st row" <|
            \_ ->
                Expect.false "returns false" (deepMember 0 3 exampleNonMatchingRangeList)
        , test "no match on 2nd row" <|
            \_ ->
                Expect.false "returns false" (deepMember 0 3 exampleNonMatchingRangeList)
        , test "bug" <|
            \_ ->
                Expect.true "returns true" (deepMember 1 1 bugRangeList)
        ]
