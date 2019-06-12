module Types exposing (..)

import Browser.Navigation as Nav
import Home
import Login
import Url
import Browser

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

type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotToken String
    | Logout
    | LoginMsg Login.Msg
    | HomeMsg Home.Msg