module Categories.Json exposing (..)

import Categories.Types exposing (..)
import Json.Decode as D

categoryDecoder : D.Decoder Category
categoryDecoder =
    D.map Category
        (D.field "code" D.string)

categoriesDecoder : D.Decoder CategoriesResult
categoriesDecoder=
    D.map CategoriesResult
        (D.field "results" <| D.list categoryDecoder)