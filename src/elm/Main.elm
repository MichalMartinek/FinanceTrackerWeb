module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import BudgetLines.Detail as BudgetLine
import BudgetLines.Form as BudgetLineForm
import Budgets.Detail as Budget
import Budgets.Form as BudgetForm
import Budgets.Settings as BudgetSettings
import Budgets.Statistics as BudgetStatistics
import Categories.List as Categories
import Common
import Home.Page as Home
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)
import Ports
import Router exposing (..)
import Task
import Types exposing (..)
import Url
import Users.Login as Login
import Users.Profile as Profile
import Users.Register as Register


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
            toRoute url

        loadProfileCmd t =
            Profile.fetchProfile t (ProfileMsg << Profile.GotProfile)

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

                ( BudgetStatistics id, Just t ) ->
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
    , Cmd.batch [ Cmd.map CategoriesMsg <| Categories.fetchCategories, redirectCmd ]
    )


urlChaned : Route -> Model -> ( Model, Cmd Msg )
urlChaned route model =
    let
        newModel =
            { model | route = route }
    in
    case ( route, model.token ) of
        ( Home, Just t ) ->
            Profile.initLoading { tagger = ProfileMsg, token = t } newModel.profile |> updateWith (\subModel lModel -> { lModel | profile = subModel }) newModel

        ( BudgetDetail id, Just t ) ->
            let
                ( newBudgetModel, nCmd ) =
                    Budget.initLoading t id newModel.budgetModel
            in
            ( { newModel | budgetModel = newBudgetModel, budgetLineForm = BudgetLineForm.init <| BudgetLineForm.NewBudgetLine id }, Cmd.map BudgetMsg nCmd )

        ( NewBudget, _ ) ->
            ( { newModel | budgetForm = BudgetForm.init }, Cmd.none )

        ( _, _ ) ->
            ( newModel, Cmd.none )


updateWith : (subModel -> Model -> Model) -> Model -> ( subModel, Cmd Msg ) -> ( Model, Cmd Msg )
updateWith toModel model ( subModel, subCmd ) =
    ( toModel subModel model
    , subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelToken =
            Maybe.withDefault "" model.token

        reloadProfileCmd t =
            Profile.fetchProfile t (ProfileMsg << Profile.GotProfile)
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

        CommonMsg commonMsg ->
            ( model, Common.update commonMsg model.key )

        LoginMsg loginMsg ->
            Login.update
                { tagger = LoginMsg
                , loginCmd = \token -> Task.perform (always <| GotToken token) (Task.succeed ())
                }
                loginMsg
                model.loginModel
                |> updateWith (\subModel lModel -> { lModel | loginModel = subModel }) model

        RegisterMsg registerMsg ->
            Register.update
                { tagger = RegisterMsg
                , afterRegisterCmd = Nav.pushUrl model.key "/login"
                }
                registerMsg
                model.registerModel
                |> updateWith (\subModel lModel -> { lModel | registerModel = subModel }) model

        HomeMsg homeMsg ->
            ( { model | homeModel = Home.update homeMsg model.homeModel }
            , Cmd.none
            )

        ProfileMsg profileMsg ->
            Profile.update profileMsg model.profile |> updateWith (\subModel lModel -> { lModel | profile = subModel }) model

        BudgetMsg budgetMsg ->
            Budget.update
                { token = modelToken
                , tagger = BudgetMsg
                , navKey = model.key
                }
                budgetMsg
                model.budgetModel
                |> updateWith (\subModel lModel -> { lModel | budgetModel = subModel }) model

        BudgetFormMsg budgetFormMsg ->
            BudgetForm.update
                { token = modelToken
                , tagger = BudgetFormMsg
                , reloadProfile = reloadProfileCmd
                , navKey = model.key
                }
                budgetFormMsg
                model.budgetForm
                |> updateWith (\subModel lModel -> { lModel | budgetForm = subModel }) model

        BudgetSettingsMsg budgetSettingsMsg ->
            BudgetSettings.update
                { token = modelToken
                , tagger = BudgetSettingsMsg
                , navKey = model.key
                , reloadCmd = \id -> Cmd.map BudgetMsg <| Budget.fetchBudget modelToken id Budget.GotBudget
                }
                budgetSettingsMsg
                model.budgetSettings
                |> updateWith (\subModel lModel -> { lModel | budgetSettings = subModel }) model

        BudgetLineMsg budgetLineMsg ->
            BudgetLine.update
                { token = modelToken
                , tagger = BudgetLineMsg
                , reloadBudget = \t id -> Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget
                }
                budgetLineMsg
                model.budgetLineModel
                |> updateWith (\subModel lModel -> { lModel | budgetLineModel = subModel }) model

        BudgetLineFormMsg budgetLineFormMsg ->
            BudgetLineForm.update
                { token = modelToken
                , tagger = BudgetLineFormMsg
                , reloadBudget = \t id -> Cmd.map BudgetMsg <| Budget.fetchBudget t id Budget.GotBudget
                , navKey = model.key
                }
                budgetLineFormMsg
                model.budgetLineForm
                |> updateWith (\subModel lModel -> { lModel | budgetLineForm = subModel }) model

        CategoriesMsg catMsg ->
            Categories.update catMsg model.categories |> updateWith (\subModel lModel -> { lModel | categories = subModel }) model

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
            Common.viewSidePanel model.profile.data |> Html.map CommonMsg

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
                        , Budget.view model.budgetModel (BudgetFormMsg << BudgetForm.InitForm) (BudgetMsg << Budget.DeleteBudget) (BudgetSettingsMsg << BudgetSettings.SetBudget) (\a b -> BudgetLineForm.InitLineForm a b |> BudgetLineFormMsg) (\a b -> BudgetLine.DeleteBudgetLine a b |> BudgetLineMsg)
                        , BudgetLineForm.viewForm model.budgetLineForm (Categories.getList model.categories) |> Html.map BudgetLineFormMsg
                        ]

                BudgetSettings id ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , BudgetSettings.view model.budgetSettings model.budgetModel.data |> Html.map BudgetSettingsMsg
                        ]

                BudgetStatistics id ->
                    div [ class "main-layout" ]
                        [ budgetsSidePanel
                        , BudgetStatistics.view model.budgetModel.data (BudgetSettingsMsg << BudgetSettings.SetBudget)
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
