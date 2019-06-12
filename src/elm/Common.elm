module Common exposing (viewNavigation)

import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Types exposing (Msg)


viewNavigation : Maybe String -> Msg -> Html Msg
viewNavigation token logoutCallback =
    let
        btn =
            case token of
                Nothing ->
                    a [ href "/login" ] [ text "Login" ]

                Just _ ->
                    button [ onClick logoutCallback ] [ text "Logout" ]
    in
    div [ class "navigation" ]
        [ div [ class "navigation__name" ] [ text "Finance Tracker" ]
        , btn
        ]
