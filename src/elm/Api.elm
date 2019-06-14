module Api exposing (..)

import Http
import Html exposing (Html, div, text)

apiUrl : String
apiUrl =
    "http://localhost:8000"


type DataWrapper a
    = Loading
    | Error Http.Error
    | Success a
    | Clear


errorLabel : Http.Error -> String
errorLabel err =
    case err of
        Http.BadUrl u ->
            "Bad URL" ++ u
    
        Http.Timeout ->
            "Request took too much time"

        Http.NetworkError ->
            "Check your connection to the network"

        Http.BadStatus i ->
            "Error " ++ (String.fromInt i)

        Http.BadBody b ->
            "Bad body " ++ b           


defaultDataWrapperView : DataWrapper a -> (a -> List (Html msg)) -> List (Html msg)
defaultDataWrapperView wrapper mapper =
    case wrapper of
        Loading ->
            [ div [] [ text "Loading" ] ]

        Error err ->
            [ div [] [ text <| errorLabel err ] ]

        Success data ->
            mapper data

        _ ->
            []
