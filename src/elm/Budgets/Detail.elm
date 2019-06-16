module Budgets.Detail exposing (Model, Msg(..), fetchBudget, init, initLoading, update, view)

import Api
import Browser.Navigation as Nav
import BudgetLines.Detail as BudgetLinesDetail exposing (viewBudgetsListItem)
import BudgetLines.Types as BudgetLinesTypes exposing (BudgetLine)
import Budgets.Common exposing (..)
import Budgets.Json exposing (budgetDecoder)
import Budgets.Types exposing (Budget, BudgetWrapper)
import Formatters
import Html exposing (Html, a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Time
import Users.Types exposing (ProfileDataWrapper, Role)



-- Model


type alias Model =
    { data : BudgetWrapper
    , delete : Api.DataWrapper Bool
    }


type Msg
    = GotBudget (Result Http.Error Budget)
    | DeleteBudget Int
    | DeletedBudget (Result Http.Error ())


initLoading :
    String
    -> Int
    -> Model
    -> ( Model, Cmd Msg )
initLoading token id model =
    ( { model | data = Api.Loading }, fetchBudget token id GotBudget )


init : Model
init =
    { data = Api.Clear
    , delete = Api.Clear
    }


update :
    { token : String
    , tagger : Msg -> msg
    , navKey : Nav.Key
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { token, tagger, navKey } msg model =
    case msg of
        GotBudget result ->
            case result of
                Ok profile ->
                    ( { model | data = Api.Success profile }, Cmd.none )

                Err err ->
                    ( { model | data = Api.Error err }, Cmd.none )

        DeleteBudget id ->
            ( { model | delete = Api.Loading }, deleteBudget token id (tagger << DeletedBudget) )

        DeletedBudget result ->
            case result of
                Ok _ ->
                    ( { model | delete = Api.Success True }, Nav.pushUrl navKey "/" )

                Err err ->
                    ( { model | delete = Api.Error err }, Cmd.none )



-- HTTP


fetchBudget : String -> Int -> (Result Http.Error Budget -> msg) -> Cmd msg
fetchBudget token id msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = Api.apiUrl ++ "/budgets/" ++ String.fromInt id ++ "/"
        , body = Http.emptyBody
        , expect = Http.expectJson msg budgetDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteBudget : String -> Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteBudget token id msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "DELETE"
        , headers = headers
        , url = Api.apiUrl ++ "/budgets/" ++ String.fromInt id ++ "/"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Views


viewBudgetsLines : Bool -> String -> List BudgetLine -> (BudgetLine -> msg) -> (Int -> msg) -> Html msg
viewBudgetsLines viewOnly currency budgets onEdit onDelete =
    div [] <|
        List.map5 viewBudgetsListItem (List.repeat (List.length budgets) viewOnly) (List.repeat (List.length budgets) currency) (List.sortWith (\a b -> compare (Time.toMillis Time.utc a.date_created) (Time.toMillis Time.utc b.date_created)) budgets) (List.repeat (List.length budgets) onEdit) (List.repeat (List.length budgets) onDelete)


viewRibbon : Int -> msg -> Bool -> Html msg
viewRibbon id settingsMsg disabledSettings =
    div [ class "budget-ribbon" ]
        [ button [ disabled True, class "btn" ] [ text "Detail" ]
        , a [ class "btn", href <| "/budget-statistics/" ++ String.fromInt id ] [ text "Statistics" ]
        , button [ class "btn", onClick settingsMsg, disabled disabledSettings ] [ text "Settings" ]
        ]


view : Model -> ProfileDataWrapper -> (Budget -> msg) -> (Int -> msg) -> (Int -> msg) -> (Int -> BudgetLine -> msg) -> (Int -> Int -> msg) -> Html msg -> List (Html msg)
view { data } profileData editMsg deleteMsg settingsMsg editLineMsg deleteLineMsg viewForm =
    Api.defaultDataWrapperView profileData <|
        \profile ->
            Api.defaultDataWrapperView data <|
                \budget ->
                    [ div [ class "main-layout__inner" ]
                        [ h2 [] [ text budget.name ]
                        , viewRibbon budget.id (settingsMsg budget.id) (not <| isAdmin budget profile.id)
                        , button [ class "btn", onClick (editMsg budget), disabled <| not <| isAdmin budget profile.id ] [ text "Edit" ]
                        , button [ class "btn", onClick (deleteMsg budget.id), disabled <| not <| isAdmin budget profile.id ] [ text "Delete" ]
                        , h2 [] [ text "Budget items" ]
                        , viewBudgetsLines (not <| isAdmin budget profile.id) budget.currency budget.lines (editLineMsg budget.id) (deleteLineMsg budget.id)
                        ]
                    , if isAdmin budget profile.id then
                        viewForm

                      else
                        div [] []
                    ]
