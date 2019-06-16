module Categories.HelpersTest exposing
    ( testTransformToName
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import Categories.Helpers exposing (transformToName)
import Categories.Types exposing (Category)


testTransformToName : Test
testTransformToName =
    describe "transformToName"
        [ test "simple code" <|
            \_ ->
                Expect.equal (transformToName {code = "SAXOPHONE"}) "Saxophone"
        , test "code in correct format" <|
            \_ ->
                Expect.equal (transformToName {code = "Two word example"}) "Two word example"
        , test "code in almost correct format" <|
            \_ ->
                Expect.equal (transformToName {code = "Two Word example"}) "Two word example"
        , test "code with underscore" <|
            \_ ->
                Expect.equal (transformToName {code = "NOT_SET"}) "Not set"
        ]
