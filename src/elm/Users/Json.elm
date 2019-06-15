module Users.Json exposing (..)

import Json.Encode as E

encodeRegisterForm : String -> String -> String -> E.Value
encodeRegisterForm password username email =
    E.object
        [ ( "password", E.string password )
        , ( "username", E.string username )
        , ( "email", E.string email )
        ]