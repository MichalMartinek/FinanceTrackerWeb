module Common exposing
    ( init
    , Model
    , Msg(..)
    , update
    , viewNavigation
    )

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Logout


type alias Model =
    { token : Maybe String
    }


init : Maybe String -> Model
init token =
    { token = token
    }

update :
    {logoutCmd : Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { logoutCmd } msg model =
    case msg of
        Logout ->
            ( { token = Maybe.Nothing }
            , logoutCmd
            )



viewNavigation : String -> Html Msg
viewNavigation username =
    div [ class "page" ]
        [ div [ class "page" ] [ text ("Test" ++ username) ], 
            button [ onClick Logout ] [ text "Logout" ]
        ]

        
