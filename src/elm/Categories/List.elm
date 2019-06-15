module Categories.List exposing (Model, Msg(..), fetchCategories, getList, init, update)

import Api
import Categories.Json exposing (..)
import Categories.Types exposing (..)
import Http


type alias Model =
    { data : Api.DataWrapper (List Category)
    }


type Msg
    = GotCategories (Result Http.Error CategoriesResult)


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
        GotCategories result ->
            case result of
                Ok data ->
                    ( { model | data = Api.Success data.results }, Cmd.none )

                Err err ->
                    ( { model | data = Api.Error err }, Cmd.none )



-- HTTP


fetchCategories : Cmd Msg
fetchCategories =
    Http.request
        { method = "GET"
        , headers = []
        , url = Api.apiUrl ++ "/categories/"
        , body = Http.emptyBody
        , expect = Http.expectJson GotCategories categoriesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getList : Model -> List Category
getList { data } =
    case data of
        Api.Success l ->
            l

        _ ->
            []
