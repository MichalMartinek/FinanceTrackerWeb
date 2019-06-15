module Budgets.Json exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Budgets.Types exposing (Budget)
import BudgetLines.Json as BudgetLinesJson exposing (budgetLineDecoder)


budgetDecoder : D.Decoder Budget
budgetDecoder =
    D.map6 Budget
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" D.string)
        (D.field "date_created" DecodeExtra.datetime)
        (D.field "date_updated" DecodeExtra.datetime)
        (D.field "lines" (D.list budgetLineDecoder))

