module Budgets.Json exposing (budgetDecoder, encodeForm)

import BudgetLines.Json as BudgetLinesJson exposing (budgetLineDecoder)
import Budgets.Types exposing (Budget)
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Json.Encode as E
import Users.Json exposing (userWithRoleDecoder)


budgetDecoder : D.Decoder Budget
budgetDecoder =
    D.map7 Budget
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" D.string)
        (D.field "date_created" DecodeExtra.datetime)
        (D.field "date_updated" DecodeExtra.datetime)
        (D.field "lines" (D.list budgetLineDecoder))
        (D.field "users" (D.list userWithRoleDecoder))


encodeForm : String -> String -> E.Value
encodeForm name currency =
    E.object
        [ ( "name", E.string name )
        , ( "currency", E.string currency )
        ]
