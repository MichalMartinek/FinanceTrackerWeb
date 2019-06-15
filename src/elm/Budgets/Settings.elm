module Budgets.Settings exposing (..)

import Api
import Browser.Navigation as Nav
import Users.Types exposing (User, Role, UserWithRole)
import Users.Json exposing (userDecoder, encodeRole, fromRole)
import Debug
import Html exposing (Html, a, button, div, form, h1, h2, input, label, span, text, textarea, p)
import Html.Attributes exposing (href, disabled, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Url
import Json.Decode as D
import List.Extra


type alias Model =
    { searchInput : String
    , searchResult : Api.DataWrapper (List User)
    , postRole : Api.DataWrapper ()
    , deleteRole : Api.DataWrapper ()
    , selected : Maybe User
    , budgetId : Maybe Int
    }




type Msg
    = SearchChanged String
    | UserSelected User
    | RoleChanged Role
    | SetBudget Int
    | DeleteRole Int
    | Submit
    | GotSearchResults (Result Http.Error (List User))
    | PostedRole (Result Http.Error ())
    | DeletedRole (Result Http.Error ())


init : Maybe Int -> Model
init budgetId =
    { searchInput = ""
    , selected = Nothing
    , searchResult = Api.Clear
    , postRole = Api.Clear
    , deleteRole = Api.Clear
    , budgetId = budgetId
    }


update :
    { token : String
    , tagger : Msg -> msg
    , navKey : Nav.Key
    , reloadCmd : Int -> Cmd msg
    }
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update { token, tagger, navKey, reloadCmd } msg model =
    case msg of
        UserSelected usr ->
            ( { model | selected = Just usr, searchResult = Api.Clear }
            , Cmd.none
            )

        SearchChanged s ->
            ( { model | searchInput = s }, Cmd.none )

        RoleChanged s ->
            ( { model | postRole = Api.Loading }, postRole token model s (tagger << PostedRole) )

        DeleteRole s ->
            ( { model | deleteRole = Api.Loading }, deleteRole token s (tagger << DeletedRole) )

        SetBudget s ->
            ( { model | budgetId = Just s }, Nav.pushUrl navKey <| "/budget-settings/" ++ (String.fromInt s) )

        Submit ->
            if String.isEmpty model.searchInput then
                ( model, Cmd.none )

            else
                ( { model | searchResult = Api.Loading }
                , searchUsers token model (tagger << GotSearchResults)
                )

        GotSearchResults result ->
            case Debug.log "budget-posted" result of
                Ok d ->
                    ( { model | searchResult = Api.Success d }, Cmd.none )

                Err err ->
                    ( { model | searchResult = Api.Error err }, Cmd.none )

        PostedRole result ->
            case Debug.log "budget-posted" result of
                Ok d ->
                    ( { model | postRole = Api.Success d }, reloadCmd <| Maybe.withDefault 0 model.budgetId )

                Err err ->
                    ( { model | postRole = Api.Error err }, Cmd.none )

        DeletedRole result ->
            case Debug.log "budget-posted" result of
                Ok d ->
                    ( { model | deleteRole = Api.Success d }, reloadCmd <| Maybe.withDefault 0 model.budgetId )

                Err err ->
                    ( { model | deleteRole = Api.Error err }, Cmd.none )


-- HTTP


searchUsers : String -> Model -> (Result Http.Error (List User) -> msg) -> Cmd msg
searchUsers token model msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
        searchTerm = Url.percentEncode model.searchInput
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = Api.apiUrl ++ "/search-user/" ++ searchTerm
        , body = Http.emptyBody
        , expect = Http.expectJson msg (D.list userDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

postRole : String -> Model -> Role -> (Result Http.Error () -> msg) -> Cmd msg
postRole token model role msg =
    case (model.budgetId, model.selected) of
        (Just budgetId, Just usr) ->
            let
                headers =
                    [ Http.header "Authorization" ("token " ++ token)
                    ]
            in
            Http.request
                { method = "POST"
                , headers = headers
                , url = Api.apiUrl ++ "/roles/"
                , body = Http.jsonBody <| encodeRole budgetId usr.id role
                , expect = Http.expectWhatever msg
                , timeout = Nothing
                , tracker = Nothing
                }
        _ ->
            
         Cmd.none 


deleteRole : String -> Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteRole token id msg =
    let
        headers =
            [ Http.header "Authorization" ("token " ++ token)
            ]
    in
    Http.request
        { method = "DELETE"
        , headers = headers
        , url = Api.apiUrl ++ "/roles/" ++ String.fromInt id ++ "/"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


-- Views

viewUser : User -> Html Msg
viewUser usr = 
    div [onClick <| UserSelected usr] [
        h2 [] [text usr.username]
    ]
    
viewSearchResult : List User -> Html Msg
viewSearchResult list =
    if List.length list == 0 then
        div [] [text "No search result"]
    else
        div [] <| List.map viewUser list

viewUserWithRole : UserWithRole -> Html Msg
viewUserWithRole usr = 
    div [] [
        h2 [] [text usr.user.username]
        , p [] [text <| fromRole usr.rel]
        , button [onClick <| DeleteRole usr.id] [text "Delete"]
    ]

filterUsers : List User -> List UserWithRole -> List User
filterUsers result assigned =
    List.filter (\ a -> Maybe.withDefault True <| Maybe.map (\_ -> False) <| List.Extra.find (\b -> a.id == b.user.id) assigned) result


view : Model -> List UserWithRole -> Html Msg
view model list =
    let
        searchResult =
            case model.searchResult of
                Api.Success l ->
                    viewSearchResult <| filterUsers l list 
            
                _ ->
                    div [] []  
        addForm = 
            case model.selected of
                Just usr ->
                    [
                        h2 [] [text <| "Selected: " ++ usr.username]
                        ,button [onClick <| RoleChanged Users.Types.Admin] [text "Set as admin"] 
                        ,button [onClick <| RoleChanged Users.Types.Viewer] [text "Set as viewer"] 
                    ]
                _ ->
                    []
        userList =
                div [] [
                    h2 [] [text "Assigned users: "]
                    , div [] <| List.map viewUserWithRole list
                ]
    in
    
    case model.budgetId of
        Nothing ->
            div [] []
        Just id ->
            div []
                [ div [] [ a [href <| "/budget/" ++ (String.fromInt id)] [text "Detail"]]
                , userList
                , h1 [] [text "Add new user"]
                , form [ onSubmit Submit ]
                    [ div []
                        [ label [] [ text "Search user" ]
                        , input [ type_ "text", value model.searchInput, onInput SearchChanged ] []
                        ]
                    , button [ type_ "submit" ] [ text "Search" ]
                    ]
                , div [] [searchResult]
                , div [] addForm
                ]

