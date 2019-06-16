module FormattersTest exposing
    ( testPadMinutes, testToUtcString
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import Formatters
import Time


testToUtcString : Test
testToUtcString =
    describe "toUtcString"
        [ test "\"random\" date" <|
            \_ ->
                Expect.equal (Formatters.toUtcString <| Time.millisToPosix 812131505000) "26.9.1995 16:05"
        , test "\"random date\" no 2" <|
            \_ ->
                Expect.equal (Formatters.toUtcString <| Time.millisToPosix 879153005000) "10.11.1997 9:10"
        ]


testPadMinutes : Test
testPadMinutes =
    describe "padMinutes"
        [ test "pad single character" <|
            \_ ->
                Expect.equal (Formatters.padMinutes "3") "03"
        , test "pad more characters" <|
            \_ ->
                Expect.equal (Formatters.padMinutes "13") "13"
        ]
