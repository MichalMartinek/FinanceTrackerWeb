module Common exposing (viewNavigation, viewSidePanel)

import Api
import Formatters
import Html exposing (Html, a, button, div, h1, h2, li, p, text, span, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Profile


viewNavigation : Maybe String -> msg -> Html msg
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


viewSidePanel : Profile.Model -> Html msg
viewSidePanel { data } =
    div [ class "sidepanel" ] <|
        Api.defaultDataWrapperView data <|
            \profile ->
                [ h2 [] [ text "Budgets" ]
                , a [ href "/budgets-new" ] [ text "Create new" ]
                , ul [ class "budgets-list" ] <| List.map viewBudgetsListItem profile.budgets
                ]


viewBudgetsListItem : Profile.BudgetWithRoleData -> Html msg
viewBudgetsListItem budget =
    li
        [ class "budgets-item" ]
        [ a [ href <| "/budgets/" ++ String.fromInt budget.budget.id ]
            [ span [class "budgets-item__title"] [ text budget.budget.name ]
            , p [] [ text <| Formatters.toUtcString budget.budget.date_created ]
            ]
        ]
