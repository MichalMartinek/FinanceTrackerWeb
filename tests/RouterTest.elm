module RouterTest exposing
    ( testToRoute
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import Router exposing (toRoute)
import Url
import Types exposing (..)


createUrl : String -> Url.Url
createUrl s =
    { protocol = Url.Http
    , host = ""
    , port_ = Nothing
    , path = s
    , query = Nothing
    , fragment = Nothing
    }

testToRoute : Test
testToRoute =
    describe "toRoute"
        [ test "Home page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/") Home
        , test "New budget page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/new-budget") NewBudget
        , test "Register page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/create-user") Register
        , test "Login page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/login") Login
        , test "Budget detail page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/budget/4") <| BudgetDetail 4
        , test "Budget settings page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/budget-settings/4") <| BudgetSettings 4
        , test "Budget statistics page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/budget-statistics/4") <| BudgetStatistics 4
        , test "Edit budget page" <|
            \_ ->
                Expect.equal (toRoute <| createUrl "/budget-edit/4") <| EditBudget 4
        ]


