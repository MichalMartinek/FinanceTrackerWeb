module Users.JsonTest exposing (testBudgetDecoder, testBudgetWithRoleDecoder, testDecodeToken, testEncodeLoginForm, testEncodeRegisterForm, testEncodeRole, testFromRole, testProfileDecoder, testToRole, testUserDecoder, testUserWithRoleDecoder)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Json.Decode as D
import Json.Encode as E
import Result
import Test exposing (..)
import Time
import Users.Json exposing (..)
import Users.Types exposing (..)


testDecodeToken : Test
testDecodeToken =
    describe "decodeToken"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString decodeToken "{ \"token\": \"ed0a71872\"}") <| Result.Ok "ed0a71872"
        ]


testEncodeLoginForm : Test
testEncodeLoginForm =
    describe "encodeLoginForm"
        [ test "encodes" <|
            \_ ->
                Expect.equal
                    (E.encode 0 <| encodeLoginForm "pass" "usr")
                    "{\"username\":\"usr\",\"password\":\"pass\"}"
        ]


testEncodeRegisterForm : Test
testEncodeRegisterForm =
    describe "encodeRegisterForm"
        [ test "encodes" <|
            \_ ->
                Expect.equal
                    (E.encode 0 <| encodeRegisterForm "pass" "usr" "mail@mail.com")
                    "{\"password\":\"pass\",\"username\":\"usr\",\"email\":\"mail@mail.com\"}"
        ]


testToRole : Test
testToRole =
    describe "toRole"
        [ test "converts Admin" <|
            \_ ->
                Expect.equal (toRole "ADMIN") Admin
        , test "converts Viewer" <|
            \_ ->
                Expect.equal (toRole "VIEWER") Viewer
        , test "converts something to default value" <|
            \_ ->
                Expect.equal (toRole "NOTHING") Viewer
        ]


testFromRole : Test
testFromRole =
    describe "fromRole"
        [ test "converts Admin" <|
            \_ ->
                Expect.equal (fromRole Admin) "ADMIN"
        , test "converts Viewer" <|
            \_ ->
                Expect.equal (fromRole Viewer) "VIEWER"
        ]


budgetJSON : String
budgetJSON =
    """
    {
        "id":250,
        "currency": "SYP",
        "name": "Syrian rescue program",
        "date_created": "2019-06-15T15:59:10.010695Z",
        "date_updated": "2019-06-15T15:59:10.010733Z"
    }
    """


budgetWithRoleJSON : String
budgetWithRoleJSON =
    "{\"budget\":" ++ budgetJSON ++ ", \"rel\": \"ADMIN\" }"


budgetData : BudgetData
budgetData =
    { id = 250
    , currency = "SYP"
    , name = "Syrian rescue program"
    , date_created = Time.millisToPosix 1560614350011
    , date_updated = Time.millisToPosix 1560614350011
    }


budgetWithRoleData : BudgetWithRoleData
budgetWithRoleData =
    { budget = budgetData, rel = Admin }


testBudgetDecoder : Test
testBudgetDecoder =
    describe "budgetDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString budgetDecoder budgetJSON) <| Result.Ok budgetData
        ]


testBudgetWithRoleDecoder : Test
testBudgetWithRoleDecoder =
    describe "budgetWithRoleDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString budgetWithRoleDecoder budgetWithRoleJSON) <| Result.Ok budgetWithRoleData
        ]


testProfileDecoder : Test
testProfileDecoder =
    describe "profileDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString profileDecoder <| "{\"budgets\": [" ++ budgetWithRoleJSON ++ "], \"username\": \"ADMIN\", \"id\": 15 }") <| Result.Ok { id = 15, username = "ADMIN", budgets = [ budgetWithRoleData ] }
        ]


testUserDecoder : Test
testUserDecoder =
    describe "userDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString userDecoder <| "{\"username\": \"ADMIN\", \"id\": 15 }") <| Result.Ok { id = 15, username = "ADMIN" }
        ]


testUserWithRoleDecoder : Test
testUserWithRoleDecoder =
    describe "userWithRoleDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString userWithRoleDecoder <| "{\"rel\": \"ADMIN\", \"user\": {\"username\": \"ADMIN\", \"id\": 15 }, \"id\": 15 }") <| Result.Ok { id = 15, user = { id = 15, username = "ADMIN" }, rel = Admin }
        ]


testEncodeRole : Test
testEncodeRole =
    describe "encodeRole"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (E.encode 0 <| encodeRole 15 10 Admin) "{\"budget_id\":15,\"user_id\":10,\"rel\":\"ADMIN\"}"
        ]
