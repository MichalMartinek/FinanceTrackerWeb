module Profile exposing (Model, Msg(..), ProfileData, init, initLoading, update, view)

import Api
import Debug
import Http
import Json.Decode as D
import Html exposing (h1, div, text)

fetchProfile : String -> Cmd Msg
fetchProfile token =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = Api.apiUrl ++ "/profile/"
        , body = Http.emptyBody
        , expect = Http.expectJson GotProfile profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias ProfileData =
    { id : Int
    , username : String
    }


type alias Model =
    { status : Api.Status
    , data : ProfileData
    }


type Msg
    = GotProfile (Result Http.Error ProfileData)


initLoading :
    String
    -> Model
    -> ( Model, Cmd Msg )
initLoading token model =
    ( { model | status = Api.Loading }, fetchProfile token )


init : Model
init =
    { status = Api.Loading
    , data =
        { id = 0
        , username = ""
        }
    }


update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProfile result ->
            case Debug.log "profile" result of
                Ok profile ->
                    ( { model | data = profile, status = Api.Success }, Cmd.none )

                Err err ->
                    ( { model | status = Api.Error err }, Cmd.none )


profileDecoder : D.Decoder ProfileData
profileDecoder =
    D.map2 ProfileData
        (D.field "id" D.int)
        (D.field "username" D.string)

view : Model -> Html.Html msg
view model =
    div [] [text ("Username : " ++ (Debug.log "asd" model).data.username)]