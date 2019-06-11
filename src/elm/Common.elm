module Common exposing
    ( Msg(..)
    , viewNavigation
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Msg
    = NoOp


viewNavigation : String -> Html Msg
viewNavigation username =
    div [ class "page" ]
        [ text ("Test" ++ username) ]
