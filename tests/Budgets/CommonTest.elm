module Budgets.CommonTest exposing (testIsAdmin)

import Budgets.Common exposing (..)
import Budgets.Types exposing (..)
import Users.Types exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Result
import Test exposing (..)
import Time



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


testIsAdmin : Test
testIsAdmin =
    describe "isAdmin"
        [ test "recognizes that user is admin" <|
            \_ ->
                Expect.equal (isAdmin budgetData 2) True
        , test "recognizes that user is not admin" <|
            \_ ->
                Expect.equal (isAdmin budgetData 4) False
        ]
