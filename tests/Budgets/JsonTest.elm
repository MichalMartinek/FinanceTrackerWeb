module Budgets.JsonTest exposing (testBudgetDecoder)

import Budgets.Json exposing (..)
import Budgets.Types exposing (..)
import Users.Types exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Json.Decode as D
import Json.Encode as E
import Result
import Test exposing (..)
import Time


budgetJSON : String
budgetJSON =
    """
    {
        "id": 1,
        "name": "Not Emptu",
        "date_created": "2019-06-07T09:07:46.085914Z",
        "date_updated": "2019-06-16T13:41:52.763038Z",
        "currency": "CZK",
        "modified_by": {
            "id": 2,
            "username": "admin",
            "email": "admin@admin.cz"
        },
        "lines": [
            {
                "description": "ads",
                "amount": "123.00",
                "category": {
                    "code": "FOOD"
                },
                "date_created": "2019-06-15T22:47:27.944301Z",
                "id": 1
            },
            {
                "description": "123",
                "amount": "1233.00",
                "category": {
                    "code": "OTHER"
                },
                "date_created": "2019-06-16T08:19:09.184078Z",
                "id": 20
            }
        ],
        "users": [
            {
                "user": {
                    "id": 2,
                    "username": "admin",
                    "email": "admin@admin.cz"
                },
                "rel": "ADMIN",
                "id": 2
            }
        ]
    }
    """


budgetData : Budget
budgetData =
    { currency = "CZK"
    , date_created = Time.millisToPosix 1559898466086
    , date_updated = Time.millisToPosix 1560692512763
    , id = 1
    , lines = [ { amount = 123, category = { code = "FOOD" }, date_created = Time.millisToPosix 1560638847944, description = "ads", id = 1 }, { amount = 1233, category = { code = "OTHER" }, date_created = Time.millisToPosix 1560673149184, description = "123", id = 20 } ]
    , name = "Not Emptu"
    , users = [ { id = 2, rel = Admin, user = { id = 2, username = "admin" } } ]
    }


testBudgetDecoder : Test
testBudgetDecoder =
    describe "budgetDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString budgetDecoder budgetJSON) <| Result.Ok budgetData
        ]



testEncodeForm : Test
testEncodeForm =
    describe "encodeRole"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (E.encode 0 <| encodeForm "Flat budget" "AFG") "{\"name\":\"Flat budget\",\"currency\":\"AFG\"}"
        ]
