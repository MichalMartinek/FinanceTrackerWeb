module Common exposing (Msg(..), update, viewBudgetsListItem, viewNavigation, viewSidePanel)

import Api
import Browser.Navigation as Nav
import Formatters
import Html exposing (Html, a, button, div, h1, h2, li, p, i, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Users.Types exposing (BudgetWithRoleData, ProfileDataWrapper)

type Msg
    = Redirect String


update : Msg -> Nav.Key -> Cmd msg
update msg navKey =
    case msg of
        Redirect s ->
            Nav.pushUrl navKey s


getTitle : String -> Html msg
getTitle url =
    a [ class "navigation__name", href url ] [ text "Finance Tracker" ]


viewUnauthorizedMenu : Html msg
viewUnauthorizedMenu =
    div []
        [ div [ class "navigation" ]
            [ div [ class "navigation__inner" ]
                [ getTitle "#"
                , div []
                    []
                ]
            ]
        , div [ class "login-menu" ]
            [ a [ class "btn", href "/login" ] [ text "Login" ]
            , a [ class "btn", href "/create-user" ] [ text "Create user" ]
            ]
        ]


viewAuthorizedMenu : msg -> Html msg
viewAuthorizedMenu logoutCallback =
    div [ class "navigation" ]
        [ div [ class "navigation__inner" ]
            [ getTitle "/"
            , div []
                [ button [ class "btn", onClick logoutCallback ] [ text "Logout" ]
                ]
            ]
        ]


viewNavigation : Maybe String -> msg -> Html msg
viewNavigation token logoutCallback =
    case token of
        Nothing ->
            viewUnauthorizedMenu

        Just _ ->
            viewAuthorizedMenu logoutCallback


viewSidePanel : ProfileDataWrapper -> Html Msg
viewSidePanel data =
    div [ class "sidepanel" ] <|
        Api.defaultDataWrapperView data <|
            \profile ->
                [ h2 [] [ text "Budgets" ]
                , a [ href "/new-budget", class "btn" ] [ text "Create new" ]
                , ul [ class "budgets-list" ] <| List.map viewBudgetsListItem profile.budgets
                ]


viewBudgetsListItem : BudgetWithRoleData -> Html Msg
viewBudgetsListItem budget =
    let
        iconClass = case budget.rel of
            Users.Types.Admin ->
                "fas fa-edit"
        
            Users.Types.Viewer ->
                "fas fa-eye"
    in
    li
        [ class "budgets-item" ]
        [ button [ class "budgets-item__btn", onClick <| Redirect <| "/budget/" ++ String.fromInt budget.budget.id ]
            [ div [ class "budgets-item__title" ] [ text budget.budget.name, i [class iconClass] [] ]
            , p [class "budgets-item__time"] [ text <| Formatters.toUtcString budget.budget.date_created ]
            ]
        ]
