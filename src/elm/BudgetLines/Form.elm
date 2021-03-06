module BudgetLines.Form exposing (FormType(..), Model, Msg(..), init, sendBudgetLine, update, viewForm)

import Api
import Browser.Navigation as Nav
import Budgets.Types exposing (Budget)
import BudgetLines.Types exposing (..)
import BudgetLines.Json exposing (..)
import Html exposing (Html, a, button, div, form, h2, input, label, span, text, textarea, select, option)
import Html.Attributes exposing (disabled, type_, value, class)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import String.Extra
import Categories.Types exposing (Category)
import Categories.Helpers exposing (transformToName)

type FormType
    = NoneSelected
    | NewBudgetLine Int
    | EditBudgetLine Int Int


type alias Model =
    { form : BudgetLineForm
    , formType : FormType
    , send : Api.DataWrapper ()
    }


type Msg
    = ClearForm
    | InitLineForm Int BudgetLine
    | DescriptionChanged String
    | AmountChanged String
    | CategoryChanged String
    | FormTypeChanged FormType
    | Submit
    | GotBudgetLine (Result Http.Error ())


getBudgetId : FormType -> Int
getBudgetId ft = 
            case ft of
                NewBudgetLine id ->
                    id

                EditBudgetLine id _ ->
                    id
                NoneSelected ->
                    0


initForm : BudgetLineForm
initForm =
    { description = ""
        , amount = 0.0
        , category = "NOT_SET"
        }

init : FormType -> Model
init ft =
    { form = initForm
    , formType = ft
    , send = Api.Clear
    }


update :
    { token : String
    , tagger : Msg -> msg
    , reloadBudget : String -> Int -> Cmd msg
    , navKey : Nav.Key
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { token, tagger, reloadBudget, navKey } msg model =
    case msg of
        ClearForm ->
            ( init <| NewBudgetLine <| getBudgetId model.formType, Cmd.none )

        InitLineForm id budgetLine ->
            ( { form =
                    { description = budgetLine.description
                    , amount = budgetLine.amount
                    , category = budgetLine.category.code
                    }
              , formType = EditBudgetLine id budgetLine.id
              , send = Api.Clear
              }
            , Cmd.none
            )

        DescriptionChanged s ->
            let
                form =
                    model.form

                updatedForm =
                    { form | description = s }
            in
            ( { model | form = updatedForm }, Cmd.none )

        AmountChanged s ->            
            let
                form =
                    model.form

                updatedForm =
                    { form | amount = String.toFloat s |> Maybe.withDefault 0 }
            in
            ( { model | form = updatedForm }, Cmd.none )

        CategoryChanged s ->
            let
                form =
                    model.form

                updatedForm =
                    { form | category = s }
            in
            ( { model | form = updatedForm }, Cmd.none )

        FormTypeChanged s ->
            ( { model | formType = s }, Cmd.none )

        Submit ->
            if model.form.amount == 0 then
                ( model, Cmd.none )

            else
                ( { model | send = Api.Loading }
                , sendBudgetLine token model (tagger << GotBudgetLine)
                )

        GotBudgetLine result ->
            case result of
                Ok d ->
                    ( init <| NewBudgetLine <| getBudgetId model.formType, reloadBudget token <| getBudgetId model.formType)

                Err err ->
                    ( { model | send = Api.Error err }, Cmd.none )

-- HTTP
sendBudgetLine : String -> Model -> (Result Http.Error () -> msg) -> Cmd msg
sendBudgetLine token model msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]

        method =
            case model.formType of
                NewBudgetLine _ ->
                    "POST"

                EditBudgetLine _ _ ->
                    "PUT"
                NoneSelected ->
                    ""
        url =
            case model.formType of
                NewBudgetLine _ ->
                    "/lines/"

                EditBudgetLine _ id  ->
                    "/lines/" ++ String.fromInt id ++ "/"
                NoneSelected ->
                    ""
    in
    Http.request
        { method = method
        , headers = headers
        , url = Api.apiUrl ++ url
        , body = Http.jsonBody <| encodeForm model.form <| getBudgetId model.formType
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }

-- View



viewCategoryItem : Category -> Html Msg
viewCategoryItem category =
    option [value category.code] [text <| transformToName category]

viewForm : Model -> List Category ->  Html Msg
viewForm model categoriesList =
    let
        title =
            case model.formType of
                NewBudgetLine _ ->
                    "Add New"

                EditBudgetLine _ _ ->
                    "Edit"
                NoneSelected ->
                    ""
    in
    div [class "budget-form"]
        [ h2 [] [ text title ]
        , form [ onSubmit Submit ]
            [ div [class "form-row"]
                [ label [] [ text "Description" ]
                , input [ type_ "text", value model.form.description, onInput (\a -> DescriptionChanged a) ] []
                ]
            , div [class "form-row"]
                [ label [] [ text "Amount" ]
                , input [ type_ "number", value (String.fromFloat model.form.amount), onInput (\a -> AmountChanged a) ] []
                ]
            , div [class "form-row"]
                [ label [] [ text "Category" ]
                , select [ value model.form.category, onInput (\a -> CategoryChanged a) ] <| List.map viewCategoryItem categoriesList
                ]
            , button [ class "btn", type_ "submit" ] [ text "Save" ]
            , button [ class "btn", onClick ClearForm ] [ text "Cancel" ]
            ]
        ]

