module Profile exposing (Model, Msg(..), ProfileData, Role, BudgetWithRoleData, fetchProfile, init, initLoading, update, view)

import Api
import Debug
import Formatters
import Html exposing (a, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Time



-- Model


type alias BudgetData =
    { id : Int
    , currency : String
    , name : String
    , date_created : Time.Posix
    , date_updated : Time.Posix
    }


type Role
    = Admin
    | Viewer


type alias BudgetWithRoleData =
    { budget : BudgetData
    , rel : Role
    }


type alias ProfileData =
    { id : Int
    , username : String
    , budgets : List BudgetWithRoleData
    }


type alias Model =
    { data : Api.DataWrapper ProfileData
    }


type Msg
    = GotProfile (Result Http.Error ProfileData)


initLoading :
    String
    -> Model
    -> ( Model, Cmd Msg )
initLoading token model =
    ( { model | data = Api.Loading }, fetchProfile token )


init : Model
init =
    { data = Api.Clear
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
                    ( { model | data = Api.Success profile }, Cmd.none )

                Err err ->
                    ( { model | data = Api.Error err }, Cmd.none )



-- JSON

toRole : String -> Role
toRole str = 
    case str of
        "ADMIN" ->
            Admin
    
        _ ->
            Viewer   

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
    D.map2 BudgetWithRoleData
        (D.field "budget" budgetDecoder)
        (D.field "rel" <| D.map toRole D.string)


profileDecoder : D.Decoder ProfileData
profileDecoder =
    D.map3 ProfileData
        (D.field "id" D.int)
        (D.field "username" D.string)
        (D.field "budgets" (D.list budgetWithRoleDecoder))



-- HTTP


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



-- Views
-- TODO: Clean up

viewBudgetsListItem : BudgetWithRoleData -> Html.Html msg
viewBudgetsListItem budget =
    div []
        [ h1 [] [ text budget.budget.name ]
        , p [] [ text <| Formatters.toUtcString budget.budget.date_created ]
        ]


viewBudgetsList : List BudgetWithRoleData -> Html.Html msg
viewBudgetsList budgets =
    div [] <|
        List.map viewBudgetsListItem budgets


view : Model -> Html.Html msg
view { data } =
    div [] <|
        Api.defaultDataWrapperView data <|
            \profile ->
                [ h2 [] [ text ("Username : " ++ (Debug.log "Profile" profile).username) ]
                , a [ href "/" ] [ text "Test" ]
                , viewBudgetsList profile.budgets
                ]
