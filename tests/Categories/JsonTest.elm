module Categories.JsonTest exposing
    ( testCategoryDecoder, testCategoriesDecoder
    )

import Result
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import Categories.Json exposing (categoryDecoder, categoriesDecoder)
import Categories.Types exposing (Category)
import Json.Decode as D


testCategoryDecoder : Test
testCategoryDecoder =
    describe "categoryDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString categoryDecoder "{ \"code\": \"TEST\"}") <| Result.Ok {code = "TEST"}
        , test "throws error" <|
            \_ ->
                Expect.err <| D.decodeString categoryDecoder "{ \"codes\": \"TEST\"}"
        ]

testCategoriesDecoder : Test
testCategoriesDecoder =
    describe "categoriesDecoder"
        [ test "decodes correct JSON" <|
            \_ ->
                Expect.equal (D.decodeString categoriesDecoder "{\"results\":[{ \"code\": \"TEST\"}]}") <| Result.Ok {results = [{code = "TEST"}]}
        ]
