module Budget exposing (Budget, Model, Msg(..), budgetDecoder, fetchBudget, init, initLoading, update, view)

import Api
import Browser.Navigation as Nav
import BudgetLines.Json as BudgetLinesJson exposing (budgetLineDecoder, categoryDecoder)
import BudgetLines.Types as BudgetLinesTypes exposing (BudgetLine, Category)
import BudgetLines.Detail as BudgetLinesDetail exposing (viewBudgetsListItem)
import Debug
import Formatters
import Html exposing (a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Profile exposing (Role)
import Time



-- Model


type alias Budget =
    { id : Int
    , currency : String
    , name : String
    , date_created : Time.Posix
    , date_updated : Time.Posix
    , lines : List BudgetLine
    }


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



-- JSON


budgetDecoder : D.Decoder Budget
budgetDecoder =
    D.map6 Budget
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" D.string)
        (D.field "date_created" DecodeExtra.datetime)
        (D.field "date_updated" DecodeExtra.datetime)
        (D.field "lines" (D.list budgetLineDecoder))



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
        List.map3 viewBudgetsListItem budgets (List.repeat (List.length budgets) onEdit) (List.repeat (List.length budgets) onDelete)


view : Model -> (Budget -> msg) -> (Int -> msg) -> (Int -> BudgetLine -> msg) -> (Int -> Int -> msg) -> Html.Html msg
view { data } editMsg deleteMsg editLineMsg deleteLineMsg =
    div [] <|
        Api.defaultDataWrapperView data <|
            \budget ->
                [ h2 [] [ text (Debug.log "Profile" budget).name ]
                , button [ class "edit-button", onClick (editMsg budget) ] [ text "Editovat" ]
                , button [ class "delete-button", onClick (deleteMsg budget.id) ] [ text "Smazat" ]
                , viewBudgetsLines budget.lines (editLineMsg budget.id) (deleteLineMsg budget.id)
                ]
