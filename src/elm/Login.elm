module Login exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Requests


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | Submit
    | GotToken (Result Http.Error String)


type alias Model =
    { username : String
    , password : String
    , failed : Bool
    }


init : Model
init =
    { username = ""
    , password = ""
    , failed = False
    }


update :
    { tagger : Msg -> msg
    , loginCmd : String -> Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { tagger, loginCmd } msg model =
    case msg of
        UsernameChanged newUsername ->
            ( { model | username = newUsername }
            , Cmd.none
            )

        PasswordChanged newPassword ->
            ( { model | password = newPassword }
            , Cmd.none
            )

        Submit ->
            if String.isEmpty model.username || String.isEmpty model.password then
                ( model, Cmd.none )

            else
                ( model
                , Requests.fetchToken model.username model.password (tagger << GotToken)
                )

        GotToken result ->
            case result of
                Ok token ->
                    ( model, loginCmd token )

                Err _ ->
                    ( { model | failed = True }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        error =
            if model.failed then
                div [ class "login-error" ]
                    [ text "Invalid credentials" ]

            else
                text ""
    in
    form [ onSubmit Submit, class "page Login" ]
        [ error
        , label [] [ text "Username" ]
        , input [ type_ "text", value model.username, onInput UsernameChanged ] []
        , label [] [ text "Password" ]
        , input [ type_ "password", value model.password, onInput PasswordChanged ] []
        , button [ type_ "submit" ] [ text "Log In" ]
        ]
