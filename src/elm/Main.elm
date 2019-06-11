module Main exposing (main)

import Common
import Browser exposing (Document)
import Browser.Navigation as Nav
import Home
import Html exposing (h1, text)
import Login
import Ports
import Task
import Url
import Debug
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe String
    , loginModel : Login.Model
    , homeModel : Home.Model
    }


type Route
    = Home
    | Login
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Login (s "login")
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotToken String
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | CommonMsg Common.Msg


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
      }
    , redirectCmd
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
            ( { model | route = toRoute url }
            , Cmd.none
            )

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

        GotToken token ->
            ( { model | token = Just token }
            , Cmd.batch
                [ Nav.pushUrl model.key "/"
                , Ports.saveToken token
                ]
            )
        _ ->
            (model, Cmd.none)


view : Model -> Document Msg
view model =
    let
        navigation = Common.viewNavigation "test" |> Html.map CommonMsg
        content =
            case model.route of
                Login ->
                    Login.view model.loginModel
                        |> Html.map LoginMsg

                Home ->
                    Home.view model.homeModel
                        |> Html.map HomeMsg

                NotFound ->
                    h1 [] [ text "Not Found" ]
    in
    { title = "Elm Webpack Boilerplate"
    , body = [ navigation, content ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
