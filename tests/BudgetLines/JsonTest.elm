module BudgetLines.JsonTest exposing (testBudgetLineDecoder, testEncodeForm)

import BudgetLines.Json exposing (..)
import BudgetLines.Types exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Json.Decode as D
import Json.Encode as E
import Result
import Test exposing (..)
import Time


budgetLineJSON : String
budgetLineJSON =
    """
    {
                "description": "ads",
                "amount": "1233.00",
                "category": {
                    "code": "OTHER"
                },
                "date_created": "2019-06-16T08:19:09.184078Z",
                "id": 20
            }
    """


budgetLineData : BudgetLine
budgetLineData =
    { amount = 1233
    , category = { code = "OTHER" }
    , date_created = Time.millisToPosix 1560673149184
    , description = "ads"
    , id = 20
    }


testBudgetLineDecoder : Test
testBudgetLineDecoder =
    describe "budgetLineDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString budgetLineDecoder budgetLineJSON) <| Result.Ok budgetLineData
        ]


testEncodeForm : Test
testEncodeForm =
    describe "encodeForm"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (E.encode 0 <| encodeForm {amount = 15.40, description = "Regular fee", category ="HOME"} 10) "{\"description\":\"Regular fee\",\"amount\":\"15.4\",\"category_id\":\"HOME\",\"budget_id\":10}"
        ]
