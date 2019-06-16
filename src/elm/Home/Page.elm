module Home.Page exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, text, h2, a, p)
import Html.Attributes exposing (class, href)


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
    div [ class "main-layout__inner" ]
        [ 
            h2 [] [text "Home"]
            , div [] [
                p [] [text "Select one from existing budgets or"]
                , a [ href "/new-budget", class "btn" ] [ text "Create new" ] 
            ]
         ]
