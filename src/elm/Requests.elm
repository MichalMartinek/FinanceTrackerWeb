module Requests exposing (fetchToken)

import Http
import Json.Decode as D
import Json.Encode as E


apiUrl : String
apiUrl =
    "http://localhost:8000"


fetchToken : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
fetchToken username password msg =
    let
        body =
            E.object
                [ ( "username", E.string username )
                , ( "password", E.string password )
                ]
    in
    Http.post
        { url = apiUrl ++ "/api-auth/"
        , body = Http.jsonBody body
        , expect = Http.expectJson msg (D.field "token" D.string)
        }
