module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Budget
import Budgets.Form as BudgetForm
import Common
import Debug
import Home
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Login
import Ports
import Profile
import Requests
import Router exposing (..)
import Task
import Types exposing (..)
import Url


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

        loadProfileCmd t =
            Cmd.map ProfileMsg <| Profile.fetchProfile t

        redirectCmd =
            case ( initialRoute, token ) of
                ( Home, Nothing ) ->
                    Nav.pushUrl key "/login"

                ( Login, Just _ ) ->
                    Nav.pushUrl key "/"

                ( BudgetDetail id, Just t ) ->
                    Cmd.batch [ loadProfileCmd t, Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget ]

                ( EditBudget id, Just t ) ->
                    Cmd.batch
                        [ loadProfileCmd t
                        , Budget.fetchBudget t
                            id
                            (\budget ->
                                case budget of
                                    Ok b ->
                                        (BudgetFormMsg << BudgetForm.InitForm) b

                                    _ ->
                                        NoOp
                            )
                        ]

                ( _, Just t ) ->
                    loadProfileCmd t

                _ ->
                    Cmd.none
    in
    ( { key = key
      , route = initialRoute
      , token = token
      , loginModel = Login.init
      , homeModel = Home.init
      , budgetForm = BudgetForm.init
      , budgetModel = Budget.init
      , profile = Profile.init
      }
    , redirectCmd
    )


urlChaned : Route -> Model -> ( Model, Cmd Msg )
urlChaned route model =
    let
        newModel =
            { model | route = route }
    in
    case ( route, model.token ) of
        ( Home, Just t ) ->
            Profile.initLoading t newModel.profile |> updateWith (\subModel lModel -> { lModel | profile = subModel }) ProfileMsg newModel

        ( BudgetDetail id, Just t ) ->
            Budget.initLoading t id newModel.budgetModel |> updateWith (\subModel lModel -> { lModel | budgetModel = subModel }) BudgetMsg newModel

        ( _, _ ) ->
            ( newModel, Cmd.none )


updateWith : (subModel -> Model -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel (Debug.log "sub" subModel) model
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelToken =
            Maybe.withDefault "" model.token

        reloadProfileCmd t =
            Cmd.map ProfileMsg <| Profile.fetchProfile t
    in
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
            Profile.update profileMsg model.profile |> updateWith (\subModel lModel -> { lModel | profile = subModel }) ProfileMsg model

        BudgetMsg budgetMsg ->
            Budget.update budgetMsg model.budgetModel |> updateWith (\subModel lModel -> { lModel | budgetModel = subModel }) BudgetMsg model

        BudgetFormMsg budgetFormMsg ->
            let
                args =
                    { token = modelToken
                    , tagger = BudgetFormMsg
                    , reloadProfile = reloadProfileCmd
                    , navKey = model.key
                    }

                ( newLoginModel, cmd ) =
                    BudgetForm.update
                        args
                        budgetFormMsg
                        model.budgetForm
            in
            ( { model | budgetForm = newLoginModel }
            , cmd
            )

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

        budgetsSidePanel =
            Common.viewSidePanel model.profile

        content =
            case model.route of
                Login ->
                    Login.view model.loginModel
                        |> Html.map LoginMsg

                Home ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , Home.view model.homeModel |> Html.map HomeMsg
                        ]

                BudgetDetail id ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , Budget.view model.budgetModel (BudgetFormMsg << BudgetForm.InitForm)
                        ]

                NewBudget ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , BudgetForm.viewForm model.budgetForm |> Html.map BudgetFormMsg
                        ]

                EditBudget _ ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , BudgetForm.viewForm model.budgetForm |> Html.map BudgetFormMsg
                        ]

                _ ->
                    h1 [] [ text "Not Found" ]
    in
    { title = "Elm Webpack Boilerplate"
    , body = [ navigation, content ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
