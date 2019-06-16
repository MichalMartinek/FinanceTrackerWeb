module Users.Profile exposing (Model, Msg(..), fetchProfile, init, initLoading, update)

import Api
import Formatters
import Html exposing (a, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as D
import Json.Decode.Extra as DecodeExtra
import Users.Types exposing (..)
import Users.Json exposing (profileDecoder)


-- Model

type alias Model =
    { data : ProfileDataWrapper
    }


type Msg
    = GotProfile (Result Http.Error ProfileData)


initLoading :
    { token : String
    , tagger : Msg -> msg
    }
    -> Model
    -> ( Model, Cmd msg )
initLoading {token, tagger} model =
    ( { model | data = Api.Loading }, fetchProfile token (tagger << GotProfile) )


init : Model
init =
    { data = Api.Clear
    }


update :
    Msg
    -> Model
    -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotProfile result ->
            case result of
                Ok profile ->
                    ( { model | data = Api.Success profile }, Cmd.none )

                Err err ->
                    ( { model | data = Api.Error err }, Cmd.none )



-- HTTP


fetchProfile : String -> (Result Http.Error ProfileData -> msg) -> Cmd msg
fetchProfile token msg =
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
        , expect = Http.expectJson msg profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


