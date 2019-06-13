module Formatters exposing (toUtcString)

import Time exposing (..)


toCzechMonth : Month -> Int
toCzechMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


toUtcString : Time.Posix -> String
toUtcString time =
    let
        zone = utc
    in
    String.fromInt (toDay zone time)
        ++ "."
        ++ String.fromInt (toCzechMonth (toMonth zone time))
        ++ "."
        ++ String.fromInt (toYear zone time)
        ++ " "
        ++ String.fromInt (toHour zone time)
        ++ ":"
        ++ String.fromInt (toMinute zone time)
        ++ ":"
        ++ String.fromInt (toSecond zone time)
