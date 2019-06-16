module BudgetLines.Detail exposing (Model, Msg(..), deleteBudgetLine, init, update, viewBudgetsListItem)

import Api
import BudgetLines.Types exposing (BudgetLine)
import Categories.Helpers exposing (transformToName)
import Formatters
import Html exposing (Html, a, button, div, h1, h2, p, span, text)
import Html.Attributes exposing (class, href, disabled)
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


viewBudgetsListItem : Bool -> String ->  BudgetLine -> (BudgetLine -> msg) -> (Int -> msg) -> Html msg
viewBudgetsListItem viewOnly currency line onEdit onDelete =
    div [ class "line-detail" ]
        [ span [ class "line-detail__title" ] [ text line.description ]
        , p [] [ text <| transformToName line.category ]
        , p [] [ text <| (String.fromFloat line.amount) ++ " " ++ currency ]
        , p [] [ text <| Formatters.toUtcString line.date_created ]
        , button [ class "btn", onClick <| onEdit line, disabled viewOnly ] [ text "Edit" ]
        , button [ class "btn", onClick <| onDelete line.id, disabled viewOnly ] [ text "Delete" ]
        ]
