module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Common
import Debug
import Home
import Html exposing (h1, div, text)
import Login
import Ports
import Requests
import Profile
import Task
import Types exposing (..)
import Url
import Helpers exposing (..)

main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init token url key =
    let
        initialRoute =
            Debug.log "value" (toRoute url)

        redirectCmd =
            case ( initialRoute, token ) of
                ( Home, Nothing ) ->
                    Nav.pushUrl key "/login"

                ( Login, Just _ ) ->
                    Nav.pushUrl key "/"

                _ ->
                    Cmd.none
    in
    ( { key = key
      , route = initialRoute
      , token = token
      , loginModel = Login.init
      , homeModel = Home.init
      , profile = Profile.init
      }
    , redirectCmd
    )

urlChaned : Route -> Model -> ( Model, Cmd Msg )
urlChaned route model =
    let 
        newModel = {model | route = route}
    in
        case (route, model.token) of
            (Home, Just t) ->
                Profile.initLoading t newModel.profile |> updateWith (\subModel lModel -> {lModel | profile = subModel}) ProfileMsg newModel
            (_, _) ->
                (newModel, Cmd.none)


updateWith : (subModel -> Model -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel (Debug.log "sub" subModel) model
    , Cmd.map toMsg subCmd
    )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChaned (toRoute url) model

        LoginMsg loginMsg ->
            let
                dispatchToken token =
                    Task.perform (always <| GotToken token) (Task.succeed ())

                ( newLoginModel, cmd ) =
                    Login.update
                        { tagger = LoginMsg
                        , loginCmd = dispatchToken
                        }
                        loginMsg
                        model.loginModel
            in
            ( { model | loginModel = newLoginModel }
            , cmd
            )

        HomeMsg homeMsg ->
            ( { model | homeModel = Home.update homeMsg model.homeModel }
            , Cmd.none
            )

        ProfileMsg profileMsg ->
            Profile.update profileMsg model.profile |> updateWith (\subModel lModel -> {lModel | profile = subModel}) ProfileMsg model

        GotToken token ->
            ( { model | token = Just token }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Ports.saveToken token
                ]
            )

        Logout ->
            ( { model | token = Nothing }
            , Cmd.batch
                [ Ports.removeToken ()
                , Nav.pushUrl model.key "/login"
                ]
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        navigation =
            Common.viewNavigation model.token Logout

        content =
            case model.route of
                Login ->
                    Login.view model.loginModel
                        |> Html.map LoginMsg

                Home ->
                    div [] [ Profile.view model.profile
                    , Home.view model.homeModel |> Html.map HomeMsg
                     ]
                    

                NotFound ->
                    h1 [] [ text "Not Found" ]
    in
    { title = "Elm Webpack Boilerplate"
    , body = [ navigation, content ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
