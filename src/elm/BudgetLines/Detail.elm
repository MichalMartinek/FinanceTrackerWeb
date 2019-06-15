module BudgetLines.Detail exposing (Model, Msg(..), deleteBudgetLine, init, update, viewBudgetsListItem)

import Api
import BudgetLines.Types exposing (BudgetLine)
import Html exposing (Html, a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { delete : Api.DataWrapper Bool
    , budgetId : Maybe Int
    }


type Msg
    = DeleteBudgetLine Int Int
    | DeletedBudgetLine (Result Http.Error ())


init : Model
init =
    { delete = Api.Clear
    , budgetId = Nothing
    }


update :
    { token : String
    , tagger : Msg -> msg
    , reloadBudget : String -> Int -> Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { token, tagger, reloadBudget } msg model =
    case msg of
        DeleteBudgetLine budgetId id ->
            ( { model | delete = Api.Loading, budgetId = Just budgetId }, deleteBudgetLine token id (tagger << DeletedBudgetLine) )

        DeletedBudgetLine result ->
            case result of
                Ok _ ->
                    ( { model | delete = Api.Success True }, reloadBudget token <| Maybe.withDefault 0 model.budgetId )

                Err err ->
                    ( { model | delete = Api.Error err }, Cmd.none )



-- HTTP


deleteBudgetLine : String -> Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteBudgetLine token id msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "DELETE"
        , headers = headers
        , url = Api.apiUrl ++ "/lines/" ++ String.fromInt id ++ "/"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Views


viewBudgetsListItem : BudgetLine -> (BudgetLine -> msg) -> (Int -> msg) -> Html msg
viewBudgetsListItem budget onEdit onDelete=
    div []
        [ h1 [] [ text budget.description ]
        , p [] [ text budget.category.code ]
        , p [] [ text <| String.fromFloat budget.amount ]
        , div []
            [ button [ class "edit-button", onClick <| onEdit budget ] [ text "Edit" ]
            , button [ class "delete-button", onClick <| onDelete budget.id ] [ text "Delete" ]
            ]
        ]
