module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Budgets.Detail as Budget
import BudgetLines.Detail as BudgetLine
import BudgetLines.Form as BudgetLineForm
import Budgets.Form as BudgetForm
import Budgets.Settings as BudgetSettings
import Categories.List as Categories
import Common
import Debug
import Home.Page as Home
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Users.Login as Login
import Users.Register as Register
import Ports
import Users.Profile as Profile
import Router exposing (..)
import Task
import Types exposing (..)
import Url
import Api


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

        loadCategoriesCmd =
            Cmd.map CategoriesMsg <| Categories.fetchCategories

        loadProfileCmd t =
            Cmd.map ProfileMsg <| Profile.fetchProfile t

        budgetLineFormType =
            case initialRoute of
                BudgetDetail id ->
                    BudgetLineForm.NewBudgetLine id

                _ ->
                    BudgetLineForm.NoneSelected

        budgetSettingsId =
            case initialRoute of
                BudgetSettings id ->
                    Just id

                _ ->
                    Nothing

        redirectCmd =
            case ( initialRoute, token ) of
                ( Home, Nothing ) ->
                    Nav.pushUrl key "/login"

                ( Login, Just _ ) ->
                    Nav.pushUrl key "/"

                ( BudgetDetail id, Just t ) ->
                    Cmd.batch [ loadProfileCmd t, Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget ]

                ( BudgetSettings id, Just t ) ->
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
      , registerModel = Register.init
      , homeModel = Home.init
      , budgetModel = Budget.init
      , budgetForm = BudgetForm.init
      , budgetSettings = BudgetSettings.init budgetSettingsId
      , budgetLineModel = BudgetLine.init
      , budgetLineForm = BudgetLineForm.init budgetLineFormType
      , categories = Categories.init
      , profile = Profile.init
      }
    , Cmd.batch [ loadCategoriesCmd, redirectCmd ]
    )


urlChaned : Route -> Model -> ( Model, Cmd Msg )
urlChaned route model =
    let
        newModel =
            { model | route = route }
    in
    case ( route, model.token ) of
        ( Home, Just t ) ->
            Profile.initLoading t newModel.profile |> updateWithCmd (\subModel lModel -> { lModel | profile = subModel }) ProfileMsg newModel

        ( BudgetDetail id, Just t ) ->
            let
                ( newBudgetModel, nCmd ) =
                    Budget.initLoading t id newModel.budgetModel

                newBudgetLineForm =
                    BudgetLineForm.init <| BudgetLineForm.NewBudgetLine id
            in
            ( { model | budgetModel = newBudgetModel, budgetLineForm = newBudgetLineForm }, Cmd.map BudgetMsg nCmd )

        ( _, _ ) ->
            ( newModel, Cmd.none )


updateWithCmd : (subModel -> Model -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWithCmd toModel toMsg model ( subModel, subCmd ) =
    ( toModel (Debug.log "sub" subModel) model
    , Cmd.map toMsg subCmd
    )


updateWith : (subModel -> Model -> Model) -> Model -> ( subModel, Cmd Msg ) -> ( Model, Cmd Msg )
updateWith toModel model ( subModel, subCmd ) =
    ( toModel (Debug.log "sub" subModel) model
    , subCmd
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
        RegisterMsg registerMsg ->
            let
                ( newLoginModel, cmd ) =
                    Register.update
                        { tagger = RegisterMsg
                        , afterRegisterCmd = Nav.pushUrl model.key "/login"
                        }
                        registerMsg
                        model.registerModel
            in
            ( { model | registerModel = newLoginModel }
            , cmd
            )
        HomeMsg homeMsg ->
            ( { model | homeModel = Home.update homeMsg model.homeModel }
            , Cmd.none
            )

        ProfileMsg profileMsg ->
            Profile.update profileMsg model.profile |> updateWithCmd (\subModel lModel -> { lModel | profile = subModel }) ProfileMsg model

        BudgetMsg budgetMsg ->
            let
                args =
                    { token = modelToken
                    , tagger = BudgetMsg
                    , navKey = model.key
                    }
            in
            Budget.update args budgetMsg model.budgetModel |> updateWith (\subModel lModel -> { lModel | budgetModel = subModel }) model

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
        
        BudgetSettingsMsg budgetSettingsMsg ->
            let
                args =
                    { token = modelToken
                    , tagger = BudgetSettingsMsg
                    , navKey = model.key
                    , reloadCmd = (\id -> Cmd.map BudgetMsg <| Budget.fetchBudget modelToken id Budget.GotBudget)
                    }

                ( newLoginModel, cmd ) =
                    BudgetSettings.update
                        args
                        budgetSettingsMsg
                        model.budgetSettings
            in
            ( { model | budgetSettings = newLoginModel }
            , cmd
            )
        BudgetLineMsg budgetLineMsg ->
            let
                args =
                    { token = modelToken
                    , tagger = BudgetLineMsg
                    , reloadBudget = \t id -> Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget
                    }
            in
            BudgetLine.update args budgetLineMsg model.budgetLineModel |> updateWith (\subModel lModel -> { lModel | budgetLineModel = subModel }) model

        BudgetLineFormMsg budgetLineFormMsg ->
            let
                args =
                    { token = modelToken
                    , tagger = BudgetLineFormMsg
                    , reloadBudget = \t id -> Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget
                    , navKey = model.key
                    }

                ( newLoginModel, cmd ) =
                    BudgetLineForm.update
                        args
                        budgetLineFormMsg
                        model.budgetLineForm
            in
            ( { model | budgetLineForm = newLoginModel }
            , cmd
            )

        CategoriesMsg catMsg ->
            let
                ( newCategoriesModel, cmd ) =
                    Categories.update catMsg model.categories
            in
            ( { model | categories = newCategoriesModel }
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
            Common.viewSidePanel model.profile.data
        userList = 
            case model.budgetModel.data of
                Api.Success d ->
                    d.users
                _ ->
                    []
                    
        content =
            case model.route of
                Login ->
                    Login.view model.loginModel
                        |> Html.map LoginMsg
                Register ->
                    Register.view model.registerModel
                        |> Html.map RegisterMsg

                Home ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , Home.view model.homeModel |> Html.map HomeMsg
                        ]

                BudgetDetail id ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , Budget.view model.budgetModel (BudgetFormMsg << BudgetForm.InitForm) (BudgetMsg << Budget.DeleteBudget) (BudgetSettingsMsg << BudgetSettings.SetBudget)  (\a b -> BudgetLineForm.InitLineForm a b |> BudgetLineFormMsg) (\a b -> BudgetLine.DeleteBudgetLine a b |> BudgetLineMsg)
                        , BudgetLineForm.viewForm model.budgetLineForm (Categories.getList model.categories) |> Html.map BudgetLineFormMsg
                        ]

                BudgetSettings id ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , BudgetSettings.view model.budgetSettings userList |> Html.map BudgetSettingsMsg
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
