module Types exposing (Model, Msg(..), Route(..))

import Browser
import Browser.Navigation as Nav
import Budget
import Budgets.Form as BudgetForm
import BudgetLines.Form as BudgetLineForm
import BudgetLines.Detail as BudgetLine
import Home
import Categories.List as Categories
import Http
import Users.Login as Login
import Users.Register as Register
import Profile
import Url


type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe String
    , loginModel : Login.Model
    , registerModel : Register.Model
    , homeModel : Home.Model
    , budgetModel : Budget.Model
    , budgetForm : BudgetForm.Model
    , budgetLineModel : BudgetLine.Model
    , budgetLineForm : BudgetLineForm.Model
    , categories : Categories.Model
    , profile : Profile.Model
    }


type Route
    = Home
    | BudgetDetail Int
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
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | HomeMsg Home.Msg
    | BudgetMsg Budget.Msg
    | BudgetFormMsg BudgetForm.Msg
    | BudgetLineMsg BudgetLine.Msg
    | BudgetLineFormMsg BudgetLineForm.Msg
    | CategoriesMsg Categories.Msg
    | ProfileMsg Profile.Msg
