module Types exposing (Model, Msg(..), Route(..))

import Browser
import Browser.Navigation as Nav
import Home
import Http
import Profile
import Login
import Url


type alias Model =
    { key : Nav.Key
    , route : Route
    , token : Maybe String
    , loginModel : Login.Model
    , homeModel : Home.Model
    , profile : Profile.Model
    }


type Route
    = Home
    | Login
    | NotFound


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotToken String
    | Logout
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg
    | ProfileMsg Profile.Msg
