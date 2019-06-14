module Budgets.Form exposing (..)

import Api
import Json.Encode as E
import Html exposing (Html, a, button, div, h1, h2, input, label, form, span, text, textarea)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Debug
import Http
import Budget

type FormType
    = NewBudget
    | EditBudget Int


type alias Model =
    { name : String
    , currency : String
    , formType : FormType
    , send : Api.DataWrapper Budget.Budget
    }


type Msg
    = ClearForm
    | NameChanged String
    | CurrencyChanged String
    | FormTypeChanged FormType
    | Submit
    | GotBudget (Result Http.Error Budget.Budget)


init : Model
init =
    {
        name = ""
        , currency = ""
        , formType = NewBudget
        , send = Api.Clear
    }


update :
    String
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update token msg model =
    case msg of
        ClearForm ->
            ( init, Cmd.none )
            
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
                ( { model | send = Api.Loading}
                , sendBudget token model
                )

        GotBudget result ->
            case Debug.log "budget-posted" result of
                Ok d ->
                    ( { init | send = Api.Success d }, Cmd.none )

                Err err ->
                    ( { model | send = Api.Error err }, Cmd.none )



encodeForm : Model -> E.Value
encodeForm m =
    E.object
        [ ( "name", E.string m.name )
        , ( "currency", E.string m.currency )
        ]



viewForm : Model -> Html Msg
viewForm model =
        form [onSubmit Submit]
        [ div []
            [ label [] [ text "Budget Name" ]
            , input [ type_ "text", value model.name, onInput (\a -> NameChanged a) ] []
            ]
        , div []
            [ label [] [ text "Budget Currency" ]
            , input [ type_ "text", value model.currency, onInput (\a -> CurrencyChanged a) ] []
            ]
        , button [ type_ "submit" ] [ text "Save" ]
        ]


sendBudget : String -> Model -> Cmd Msg
sendBudget token model =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = Api.apiUrl ++ "/budgets/"
        , body = Http.jsonBody <| encodeForm model
        , expect = Http.expectJson GotBudget Budget.budgetDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

