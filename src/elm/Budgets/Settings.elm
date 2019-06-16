module Budgets.Settings exposing (Model, Msg(..), deleteRole, filterUsers, init, postRole, searchUsers, update, view, viewSearchResult, viewUser, viewUserWithRole)

import Api
import Browser.Navigation as Nav
import Budgets.Types exposing (BudgetWrapper)
import Debug
import Html exposing (Html, a, button, div, form, h1, h2, h3, input, label, p, span, text, textarea)
import Html.Attributes exposing (class, disabled, href, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import List.Extra
import Url
import Users.Json exposing (encodeRole, fromRole, userDecoder)
import Users.Types exposing (Role, User, UserWithRole)


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
            ( { model | budgetId = Just s }, Nav.pushUrl navKey <| "/budget-settings/" ++ String.fromInt s )

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
                    ( init model.budgetId, reloadCmd <| Maybe.withDefault 0 model.budgetId )

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

        searchTerm =
            Url.percentEncode model.searchInput
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
    case ( model.budgetId, model.selected ) of
        ( Just budgetId, Just usr ) ->
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
    div [ onClick <| UserSelected usr, class "user-item" ]
        [ h2 [] [ text usr.username ]
        ]


viewSearchResult : List User -> Html Msg
viewSearchResult list =
    if List.length list == 0 then
        div [] [ text "No one found" ]

    else
        div [] <| h3 [] [text "Select"] :: List.map viewUser list


viewUserWithRole : UserWithRole -> Html Msg
viewUserWithRole usr =
    div []
        [ h2 [] [ text usr.user.username ]
        , p [] [ text <| fromRole usr.rel ]
        , button [ onClick <| DeleteRole usr.id, class "btn" ] [ text "Delete" ]
        ]


filterUsers : List User -> List UserWithRole -> List User
filterUsers result assigned =
    List.filter (\a -> Maybe.withDefault True <| Maybe.map (\_ -> False) <| List.Extra.find (\b -> a.id == b.user.id) assigned) result


viewRibbon : Int -> Html msg
viewRibbon id =
    div [ class "budget-ribbon" ]
        [ a [ class "btn", href <| "/budget/" ++ String.fromInt id ] [ text "Detail" ]
        , a [ class "btn", href <| "/budget-statistics/" ++ String.fromInt id ] [ text "Statistics" ]
        , button [ disabled True, class "btn" ] [ text "Settings" ]
        ]


view : Model -> BudgetWrapper -> Html Msg
view model budgetData =
    case budgetData of
        Api.Success budget ->
            let
                searchResult =
                    case model.searchResult of
                        Api.Success l ->
                            viewSearchResult <| filterUsers l budget.users

                        _ ->
                            div [] []

                addForm =
                    case model.selected of
                        Just usr ->
                            [ h2 [] [ text <| "Selected: " ++ usr.username ]
                            , button [ onClick <| RoleChanged Users.Types.Admin, class "btn" ] [ text "Set as admin" ]
                            , button [ onClick <| RoleChanged Users.Types.Viewer, class "btn" ] [ text "Set as viewer" ]
                            ]

                        _ ->
                            []

                userList =
                    div []
                        [ h2 [] [ text "Assigned users: " ]
                        , div [ class "userlist" ] <| List.map viewUserWithRole budget.users
                        ]
            in
            div [ class "main-layout__inner" ]
                [ h2 [] [ text budget.name ]
                , viewRibbon budget.id
                , userList
                , h2 [] [ text "Add new user" ]
                , form [ onSubmit Submit ]
                    [ div [ class "form-row" ]
                        [ label [] [ text "Search user" ]
                        , input [ type_ "text", value model.searchInput, onInput SearchChanged ] []
                        , button [ type_ "submit", class "btn" ] [ text "Search" ]
                        ]
                    ]
                , div [] [ searchResult ]
                , div [] addForm
                ]

        _ ->
            div [] []
