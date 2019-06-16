module Users.Register exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Api
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Users.Json exposing (encodeRegisterForm)


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | EmailChanged String
    | Submit
    | GotResult (Result Http.Error ())


type alias Model =
    { email : String
    , username : String
    , password : String
    , error : Maybe String
    }


init : Model
init =
    { email = ""
    , username = ""
    , password = ""
    , error = Nothing
    }


update :
    { tagger : Msg -> msg
    , afterRegisterCmd : Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { tagger, afterRegisterCmd } msg model =
    case msg of
        UsernameChanged newUsername ->
            ( { model | username = newUsername }
            , Cmd.none
            )

        PasswordChanged newPassword ->
            ( { model | password = newPassword }
            , Cmd.none
            )

        EmailChanged newEmail ->
            ( { model | email = newEmail }
            , Cmd.none
            )

        Submit ->
            if String.isEmpty model.username || String.isEmpty model.password then
                ( model, Cmd.none )

            else
                ( model
                , postUser model (tagger << GotResult)
                )

        GotResult result ->
            case result of
                Ok _ ->
                    ( init, afterRegisterCmd )

                Err e ->
                    ( { model | error = Just <| Api.errorLabel e }, Cmd.none )



-- HTTP


postUser : Model -> (Result Http.Error () -> msg) -> Cmd msg
postUser model msg =
    let
        body =
            encodeRegisterForm model.password model.username model.email
    in
    Http.post
        { url = Api.apiUrl ++ "/users/"
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        }



-- Views


view : Model -> Html Msg
view model =
    let
        error =
            case model.error of
                Just errorText ->
                    div [ class "login-error" ] [ text errorText ]

                option2 ->
                    div [] []
    in
    form [ onSubmit Submit, class "page Login" ]
        [ error
        , div [ class "form-row" ]
            [ label [] [ text "Username" ]
            , input [ type_ "text", value model.username, onInput UsernameChanged ] []
            ]
        , div [ class "form-row form-row--email" ]
            [ label [] [ text "Email" ]
            , input [ type_ "email", value model.email, onInput EmailChanged ] []
            ]
        , div [ class "form-row" ]
            [ label [] [ text "Password" ]
            , input [ type_ "password", value model.password, onInput PasswordChanged ] []
            ]
        , button [ type_ "submit", class "btn" ] [ text "Create" ]
        ]
