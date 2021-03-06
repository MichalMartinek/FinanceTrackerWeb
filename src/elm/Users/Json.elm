module Users.Json exposing (..)

import Json.Encode as E
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Users.Types exposing (..)

encodeRegisterForm : String -> String -> String -> E.Value
encodeRegisterForm password username email =
    E.object
        [ ( "password", E.string password )
        , ( "username", E.string username )
        , ( "email", E.string email )
        ]
    
encodeLoginForm : String -> String -> E.Value
encodeLoginForm password username =
        E.object
                [ ( "username", E.string username )
                , ( "password", E.string password )
                ]

decodeToken : D.Decoder String
decodeToken = (D.field "token" D.string)



toRole : String -> Role
toRole str = 
    case str of
        "ADMIN" ->
            Admin
    
        _ ->
            Viewer   

fromRole : Role -> String
fromRole str = 
    case str of
        Admin ->
            "ADMIN"
    
        Viewer ->
            "VIEWER"          

budgetDecoder : D.Decoder BudgetData
budgetDecoder =
    D.map5 BudgetData
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" D.string)
        (D.field "date_created" DecodeExtra.datetime)
        (D.field "date_updated" DecodeExtra.datetime)


budgetWithRoleDecoder : D.Decoder BudgetWithRoleData
budgetWithRoleDecoder =
    D.map2 BudgetWithRoleData
        (D.field "budget" budgetDecoder)
        (D.field "rel" <| D.map toRole D.string)


profileDecoder : D.Decoder ProfileData
profileDecoder =
    D.map3 ProfileData
        (D.field "id" D.int)
        (D.field "username" D.string)
        (D.field "budgets" (D.list budgetWithRoleDecoder))


userDecoder : D.Decoder User
userDecoder =
    D.map2 User
        (D.field "id" D.int)
        (D.field "username" D.string)

userWithRoleDecoder : D.Decoder UserWithRole
userWithRoleDecoder =
    D.map3 UserWithRole
        (D.field "user" userDecoder)
        (D.field "rel" <| D.map toRole D.string)
        (D.field "id" <| D.int)


encodeRole : Int -> Int -> Role -> E.Value
encodeRole budgetId userId role =
        E.object
                [ ( "budget_id", E.int budgetId )
                , ( "user_id", E.int userId )
                , ( "rel", E.string <| fromRole role )
                ]