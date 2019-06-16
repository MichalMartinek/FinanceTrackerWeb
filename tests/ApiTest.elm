module ApiTest exposing
    ( testDefaultDataWrapperView
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import Html exposing (div, text)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, classes)
import Formatters
import Api exposing (defaultDataWrapperView)
import Http

loading : Api.DataWrapper ()
loading = Api.Loading


error : Api.DataWrapper ()
error = Api.Error <| Http.Timeout

success : Api.DataWrapper ()
success = Api.Success ()

clear : Api.DataWrapper ()
clear = Api.Clear

testDefaultDataWrapperView : Test
testDefaultDataWrapperView =
    describe "defaultDataWrapperView"
        [ test "loading" <|
            \_ ->
                defaultDataWrapperView loading (\_ -> [])
                |> div []
                |> Query.fromHtml
                |> Query.contains [ text "Loading" ]
        , test "error" <|
            \_ ->
                defaultDataWrapperView error (\_ -> [])
                |> div []
                |> Query.fromHtml
                |> Query.contains [ text <| "Request took too much time" ]
        , test "success" <|
            \_ ->
                defaultDataWrapperView success (\_ -> [text "Success"])
                |> div []
                |> Query.fromHtml
                |> Query.contains [ text <| "Success" ]
        , test "clear" <|
            \_ ->
                Expect.equal  (List.length <| defaultDataWrapperView clear (\_ -> [])) 0
                 
        ]
