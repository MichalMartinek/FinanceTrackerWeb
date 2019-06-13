module Profile exposing (Model, Msg(..), ProfileData, fetchProfile, init, initLoading, update, view)

import Api
import Debug
import Html exposing (div, h1, text, p)
import Http
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Time
import Formatters

fetchProfile : String -> Cmd Msg
fetchProfile token =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = Api.apiUrl ++ "/profile/"
        , body = Http.emptyBody
        , expect = Http.expectJson GotProfile profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias BudgetData =
    { id : Int
    , currency : String
    , name : String
    , date_created : Time.Posix
    , date_updated : Time.Posix
    }


type alias BudgetWithRoleData =
    { budget : BudgetData
    }


type alias ProfileData =
    { id : Int
    , username : String
    , budgets : List BudgetWithRoleData
    }


type alias Model =
    { status : Api.Status
    , data : ProfileData
    }


type Msg
    = GotProfile (Result Http.Error ProfileData)


initLoading :
    String
    -> Model
    -> ( Model, Cmd Msg )
initLoading token model =
    ( { model | status = Api.Loading }, fetchProfile token )


init : Model
init =
    { status = Api.Loading
    , data =
        { id = 0
        , username = ""
        , budgets = []
        }
    }


update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProfile result ->
            case Debug.log "profile" result of
                Ok profile ->
                    ( { model | data = profile, status = Api.Success }, Cmd.none )

                Err err ->
                    ( { model | status = Api.Error err }, Cmd.none )


budgetDecoder : D.Decoder BudgetData
budgetDecoder =
    D.map5 BudgetData
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" D.string)
        (D.field "date_created" DecodeExtra.datetime)
        (D.field "date_updated" DecodeExtra.datetime)


budgetWithRoleDecoder : D.Decoder BudgetWithRoleData
budgetWithRoleDecoder =
    D.map BudgetWithRoleData
        (D.field "budget" budgetDecoder)


profileDecoder : D.Decoder ProfileData
profileDecoder =
    D.map3 ProfileData
        (D.field "id" D.int)
        (D.field "username" D.string)
        (D.field "budgets" (D.list budgetWithRoleDecoder))


viewBudgetsListItem : BudgetWithRoleData -> Html.Html msg
viewBudgetsListItem budget =
    div [] [
        h1 [] [ text budget.budget.name ]
        ,p [] [ text <| Formatters.toUtcString budget.budget.date_created ]
    ]


viewBudgetsList : Model -> Html.Html msg
viewBudgetsList { data } =
    div [] <|
        List.map viewBudgetsListItem data.budgets


view : Model -> Html.Html msg
view model =
    div []
        [ div [] [ text ("Username : " ++ (Debug.log "asd" model).data.username) ]
        , viewBudgetsList model
        ]
