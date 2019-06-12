module Helpers exposing (..)
import Url
import Types exposing (..)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)

route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Login (s "login")
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)

