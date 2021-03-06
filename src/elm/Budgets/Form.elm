module Budgets.Form exposing (FormType(..), Model, Msg(..), init, sendBudget, update, viewForm)

import Api
import Browser.Navigation as Nav
import Budgets.Types exposing (Budget)
import Budgets.Json exposing (budgetDecoder, encodeForm)
import Html exposing (Html, a, button, div, form, h1, h2, input, label, span, text, textarea)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http


type FormType
    = NewBudget
    | EditBudget Int


type alias Model =
    { name : String
    , currency : String
    , formType : FormType
    , send : Api.DataWrapper Budget
    }


type Msg
    = ClearForm
    | InitForm Budget
    | NameChanged String
    | CurrencyChanged String
    | FormTypeChanged FormType
    | Submit
    | GotBudget (Result Http.Error Budget)


init : Model
init =
    { name = ""
    , currency = ""
    , formType = NewBudget
    , send = Api.Clear
    }


update :
    { token : String
    , tagger : Msg -> msg
    , reloadProfile : String -> Cmd msg
    , navKey : Nav.Key
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { token, tagger, reloadProfile, navKey } msg model =
    case msg of
        ClearForm ->
            ( init, Cmd.none )

        InitForm budget ->
            ( { name = budget.name
              , currency = budget.currency
              , formType = EditBudget budget.id
              , send = Api.Clear
              }
            , Nav.pushUrl navKey ("/budget-edit/" ++ String.fromInt budget.id)
            )

        NameChanged s ->
            ( { model | name = s }, Cmd.none )

        CurrencyChanged s ->
            ( { model | currency = s }, Cmd.none )

        FormTypeChanged s ->
            ( { model | formType = s }, Cmd.none )

        Submit ->
            if String.isEmpty model.name || String.isEmpty model.currency then
                ( model, Cmd.none )

            else
                ( { model | send = Api.Loading }
                , sendBudget token model (tagger << GotBudget)
                )

        GotBudget result ->
            case result of
                Ok d ->
                    ( { init | send = Api.Success d }, Cmd.batch [ reloadProfile token, Nav.pushUrl navKey ("/budget/" ++ String.fromInt d.id) ] )

                Err err ->
                    ( { model | send = Api.Error err }, Cmd.none )




viewForm : Model -> Html Msg
viewForm model =
    let 
        title = case model.formType of
                NewBudget ->
                    "Add new budget "

                EditBudget _ ->
                    "Edit budget"
    in
    div [class "main-layout__inner"]
        [ h2 [] [text title]
        , form [ onSubmit Submit ]
            [ div [class "form-row"]
                [ label [] [ text "Budget Name" ]
                , input [ type_ "text", value model.name, onInput (\a -> NameChanged a) ] []
                ]
            , div [class "form-row"]
                [ label [] [ text "Budget Currency" ]
                , input [ type_ "text", value model.currency, onInput (\a -> CurrencyChanged a) ] []
                ]
            , button [ type_ "submit", class "btn" ] [ text "Save" ]
            ]
        ]


sendBudget : String -> Model -> (Result Http.Error Budget -> msg) -> Cmd msg
sendBudget token model msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]

        method =
            case model.formType of
                NewBudget ->
                    "POST"

                EditBudget _ ->
                    "PUT"

        url =
            case model.formType of
                NewBudget ->
                    "/budgets/"

                EditBudget id ->
                    "/budgets/" ++ (String.fromInt id) ++ "/"
    in
    Http.request
        { method = method
        , headers = headers
        , url = Api.apiUrl ++ url
        , body = Http.jsonBody <| encodeForm model.name model.currency
        , expect = Http.expectJson msg budgetDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
