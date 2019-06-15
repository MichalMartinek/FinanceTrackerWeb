module Budgets.Detail exposing (Model, Msg(..), fetchBudget, init, initLoading, update, view)

import Api
import Browser.Navigation as Nav
import BudgetLines.Detail as BudgetLinesDetail exposing (viewBudgetsListItem)
import BudgetLines.Types as BudgetLinesTypes exposing (BudgetLine)
import Budgets.Json exposing (budgetDecoder)
import Budgets.Types exposing (Budget)
import Debug
import Formatters
import Html exposing (a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Time
import Users.Types exposing (Role)



-- Model


type alias Model =
    { data : Api.DataWrapper Budget
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
            case Debug.log "budget" result of
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


viewBudgetsLines : List BudgetLine -> (BudgetLine -> msg) -> (Int -> msg) -> Html.Html msg
viewBudgetsLines budgets onEdit onDelete =
    div [] <|
        List.map3 viewBudgetsListItem (List.sortWith (\a b -> compare (Time.toMillis Time.utc a.date_created) (Time.toMillis Time.utc b.date_created)) budgets) (List.repeat (List.length budgets) onEdit) (List.repeat (List.length budgets) onDelete)


view : Model -> (Budget -> msg) -> (Int -> msg) -> (Int -> msg) -> (Int -> BudgetLine -> msg) -> (Int -> Int -> msg) -> Html.Html msg
view { data } editMsg deleteMsg settingsMsg editLineMsg deleteLineMsg =
    div [] <|
        Api.defaultDataWrapperView data <|
            \budget ->
                [ h2 [] [ text (Debug.log "Profile" budget).name ]
                , button [ class "edit-button", onClick (editMsg budget) ] [ text "Editovat" ]
                , button [ class "delete-button", onClick (deleteMsg budget.id) ] [ text "Smazat" ]
                , button [ class "delete-button", onClick (settingsMsg budget.id) ] [ text "Settings" ]
                , viewBudgetsLines budget.lines (editLineMsg budget.id) (deleteLineMsg budget.id)
                ]
