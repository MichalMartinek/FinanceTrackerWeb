module BudgetLines.Json exposing (..)

import BudgetLines.Types exposing (..)
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra

import Categories.Json exposing (categoryDecoder)

budgetLineDecoder : D.Decoder BudgetLine
budgetLineDecoder =
    D.map5 BudgetLine
        (D.field "id" D.int)
        (D.field "description" D.string)
        (D.field "amount" DecodeExtra.parseFloat)
        (D.field "category" categoryDecoder)
        (D.field "date_created" DecodeExtra.datetime)


encodeForm : BudgetLineForm -> Int -> E.Value
encodeForm m id =
    E.object
        [ ( "description", E.string m.description )
        , ( "amount", E.string <| String.fromFloat m.amount )
        , ( "category_id", E.string m.category )
        , ( "budget_id", E.int id  )
        ]