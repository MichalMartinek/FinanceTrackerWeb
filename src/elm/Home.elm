module Home exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Msg
    = NoOp


type alias Model =
    {}


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ text "Home" ]
