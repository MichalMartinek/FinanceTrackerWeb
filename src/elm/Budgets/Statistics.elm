module Budgets.Statistics exposing (..)

import Budgets.Types exposing (Budget, BudgetWrapper)
import Html exposing (Html, a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Api
import List.Extra

import Array exposing (Array)
import Color exposing (Color)
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)


w : Float
w =
    990


h : Float
h =
    504


colors : Int -> Array Color
colors count =
    Array.fromList <| List.take count <| List.concat <| List.repeat (count // 10 + 1) [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        , Color.rgb255 107 72 107
        , Color.rgb255 159 92 85
        , Color.rgb255 208 116 60
        , Color.rgb255 255 96 0
        , Color.rgb255 255 96 120
        , Color.rgb255 155 26 13
        , Color.rgb255 10 208 130
        ]


radius : Float
radius =
    min w h / 2


viewPie : List ( String, Float ) -> Svg msg
viewPie model =
    let
        pieData =
            model |> List.map Tuple.second |> Shape.pie { defaultPieConfig | outerRadius = radius }

        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index <| colors <| List.length model, stroke Color.white ]

        makeLabel slice ( label, value ) =
            let
                ( x, y ) =
                    Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }
            in
            text_
                [ transform [ Translate x y ]
                , dy (em 0.35)
                , textAnchor AnchorMiddle
                ]
                [ text label ]
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap makeSlice pieData
            , g [] <| List.map2 makeLabel pieData model
            ]
        ]


data : List ( String, Float )
data =
    [ ( "/notifications", 2704659 )
    , ( "/about", 4499890 )
    , ( "/product", 2159981 )
    , ( "/blog", 3853788 )
    , ( "/shop", 14106543 )
    , ( "/profile", 8819342 )
    , ( "/", 612463 )
    ]

getCategories : Budget -> List String
getCategories b =
    List.Extra.unique <| List.map (\a -> a.category.code) b.lines

countCategoriesItem : String -> Budget -> Float
countCategoriesItem cat b =
    List.sum <| List.map .amount <| List.filter (\a -> a.category.code == cat) b.lines 

getCategoriesCount : Budget -> List ( String, Float )
getCategoriesCount b = 
    let
        categories = getCategories b
    in
        List.map (\a -> (a ++ " " ++ (String.fromFloat <| countCategoriesItem a b), countCategoriesItem a b)) categories

view : BudgetWrapper -> Html msg
view budget =
    case budget of
        Api.Success b ->
            div [] [
                h1 [] [text "Statistics"]
                , a [href <| "/budget/" ++ (String.fromInt b.id)] [text "Detail"]
                , a [href <| "/budget-settings/" ++ (String.fromInt b.id)] [text "Settings"]
                , a [href "#" ] [text "Statistic"]
                , div [class "full-width"] [viewPie <| getCategoriesCount b]
            ]
    
        _ ->
            div [] []
            

