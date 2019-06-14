module Helpers exposing (..)
import Url
import Types exposing (..)
import Url.Parser exposing (..)

route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map BudgetDetail (s "budget" </> int)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)

