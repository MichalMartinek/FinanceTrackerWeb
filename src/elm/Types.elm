module Types exposing (Model, Msg(..), Route(..))

import Browser
import Browser.Navigation as Nav
import Budgets.Detail as Budget
import Budgets.Form as BudgetForm
import Budgets.Settings as BudgetSettings
import BudgetLines.Form as BudgetLineForm
import BudgetLines.Detail as BudgetLine
import Home.Page as Home
import Categories.List as Categories
import Http
import Users.Login as Login
import Users.Register as Register
import Users.Profile as Profile
import Url
import Common

type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe String
    , loginModel : Login.Model
    , registerModel : Register.Model
    , homeModel : Home.Model
    , budgetModel : Budget.Model
    , budgetForm : BudgetForm.Model
    , budgetSettings : BudgetSettings.Model
    , budgetLineModel : BudgetLine.Model
    , budgetLineForm : BudgetLineForm.Model
    , categories : Categories.Model
    , profile : Profile.Model
    }


type Route
    = Home
    | BudgetDetail Int
    | BudgetSettings Int
    | BudgetStatistics Int
    | EditBudget Int
    | NewBudget
    | Login
    | Register
    | NotFound


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotToken String
    | Logout
    | CommonMsg Common.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | HomeMsg Home.Msg
    | BudgetMsg Budget.Msg
    | BudgetFormMsg BudgetForm.Msg
    | BudgetSettingsMsg BudgetSettings.Msg
    | BudgetLineMsg BudgetLine.Msg
    | BudgetLineFormMsg BudgetLineForm.Msg
    | CategoriesMsg Categories.Msg
    | ProfileMsg Profile.Msg
