module Budget exposing (Model, Msg(..), fetchBudget, init, initLoading, update, view)

import Api
import Debug
import Formatters
import Html exposing (a, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Time
import Profile exposing (Role)


-- Model
type alias Category =
    {
        code : String
    }

type alias BudgetLine =
    { id : Int
    , description : String
    , amount : Float
    , category : Category
    }

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
    }


type Msg
    = GotBudget (Result Http.Error Budget)


initLoading :
    String
    -> Int
    -> Model
    -> ( Model, Cmd Msg )
initLoading token id model =
    ( { model | data = Api.Loading }, fetchBudget token id )


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
        GotBudget result ->
            case Debug.log "budget" result of
                Ok profile ->
                    ( { model | data = Api.Success profile }, Cmd.none )

                Err err ->
                    ( { model | data = Api.Error err }, Cmd.none )



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


budgetLineDecoder : D.Decoder BudgetLine
budgetLineDecoder =
    D.map4 BudgetLine
        (D.field "id" D.int)
        (D.field "description" D.string)
        (D.field "amount" DecodeExtra.parseFloat)
        (D.field "category" categoryDecoder)


categoryDecoder : D.Decoder Category
categoryDecoder =
    D.map Category
        (D.field "code" D.string)



-- HTTP


fetchBudget : String -> Int -> Cmd Msg
fetchBudget token id =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = Api.apiUrl ++ "/budgets/" ++ ( String.fromInt id) ++ "/"
        , body = Http.emptyBody
        , expect = Http.expectJson GotBudget budgetDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- Views

viewBudgetsListItem : BudgetLine -> Html.Html msg
viewBudgetsListItem budget =
    div []
        [ h1 [] [ text budget.description ]
        , p [] [ text <| String.fromFloat budget.amount ]
        ]


viewBudgetsLines : List BudgetLine -> Html.Html msg
viewBudgetsLines budgets =
    div [] <|
        List.map viewBudgetsListItem budgets


view : Model -> Html.Html msg
view { data } =
    div [] <|
        Api.defaultDataWrapperView data <|
            \budget ->
                [ h2 [] [ text (Debug.log "Profile" budget).name ]
                , a [ href "/" ] [ text "Test" ]
                , viewBudgetsLines budget.lines
                ]
