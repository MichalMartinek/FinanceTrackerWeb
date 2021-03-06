module Budgets.Statistics exposing (view)

import Api
import Array exposing (Array)
import Axis
import Budgets.Common exposing (..)
import BudgetLines.Types exposing (BudgetLine)
import Budgets.Types exposing (Budget, BudgetWrapper)
import Color exposing (Color)
import Formatters
import Html exposing (Html, a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, disabled, href)
import Html.Events exposing (onClick)
import List.Extra
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Scale.Color
import Shape exposing (defaultPieConfig)
import SubPath exposing (SubPath)
import Time
import TypedSvg exposing (circle, g, line, rect, svg, text_)
import TypedSvg.Attributes as Explicit exposing (dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em, percent)
import Users.Types exposing (ProfileDataWrapper)



-- Curve


wc : Float
wc =
    990


hc : Float
hc =
    450


padding : Float
padding =
    100


xScale : Float -> Float -> ContinuousScale Float
xScale min max =
    Scale.linear ( padding, wc - padding ) ( min, max )


yScale : Float -> Float -> ContinuousScale Float
yScale min max =
    Scale.linear ( hc - padding, padding ) ( min, max )


type alias Curve =
    List ( Float, Float ) -> SubPath


viewCurve : ( String, Curve, Color ) -> Budget -> Svg msg
viewCurve ( name, curve, color ) b =
    let
        list =
            getSumLines b

        minimumY =
            Maybe.withDefault 0 <| List.minimum <| List.map Tuple.second list

        maximumY =
            Maybe.withDefault 0 <| List.maximum <| List.map Tuple.second list

        preparedXScale =
            xScale 0 <| toFloat <| List.length list - 1

        preparedYScale =
            yScale minimumY maximumY

        scaledPoints =
            preparedPoints list preparedXScale preparedYScale

        yLabelTicks =
            if List.length list > 5 then
                5

            else
                List.length list - 1
    in
    div []
        [ svg [ viewBox 0 0 w h ]
            [ g [ transform [ Translate 50 0 ] ]
                [ Axis.left [ Axis.tickSizeInner 12, Axis.tickSizeOuter 15 ] preparedYScale
                ]
            , g [ transform [ Translate 0 (h - 100) ] ]
                [ Axis.bottom [ Axis.tickFormat (\a -> getLabel b a), Axis.tickCount yLabelTicks, Axis.tickSizeInner 12, Axis.tickSizeOuter 15 ] preparedXScale
                ]
            , g []
                [ List.map Just scaledPoints
                    |> Shape.line curve
                    |> (\path -> Path.element path [ stroke color, fill FillNone, strokeWidth 2 ])
                ]
            ]
        ]


preparedPoints : List ( Float, Float ) -> ContinuousScale Float -> ContinuousScale Float -> List ( Float, Float )
preparedPoints list xScl yScl =
    List.map (\( x, y ) -> ( Scale.convert xScl x, Scale.convert yScl y )) list


getSum : Budget -> Int -> Float
getSum b i =
    List.sum <| List.map .amount <| List.take (i + 1) b.lines


getSumLines : Budget -> List ( Float, Float )
getSumLines b =
    List.indexedMap (\a line -> ( toFloat a, getSum b a )) b.lines


getLabel : Budget -> Float -> String
getLabel b index =
    let
        val =
            List.Extra.getAt (round index) b.lines
    in
    case val of
        Just line ->
            Formatters.toUtcString line.date_created

        Nothing ->
            ""



-- Pie


w : Float
w =
    990


h : Float
h =
    504


colors : Int -> Array Color
colors count =
    Array.fromList <|
        List.take count <|
            List.concat <|
                List.repeat (count // 10 + 1)
                    [ Color.rgb255 152 171 198
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


getCategories : Budget -> (BudgetLine -> Bool) -> List String
getCategories b typeFilter =
    List.Extra.unique <| List.map (\a -> a.category.code) <| List.filter typeFilter b.lines


countCategoriesItem : String -> Budget -> (BudgetLine -> Bool) -> Float
countCategoriesItem cat b typeFilter =
    List.sum <| List.map .amount <| List.filter (\a -> a.category.code == cat) <| List.filter typeFilter b.lines


getCategoriesCount : Budget -> (BudgetLine -> Bool) -> List ( String, Float )
getCategoriesCount b typeFilter =
    let
        categories =
            getCategories b typeFilter
    in
    List.map (\a -> ( a ++ " " ++ (String.fromFloat <| countCategoriesItem a b typeFilter), countCategoriesItem a b typeFilter )) categories


viewRibbon : Int -> msg -> Bool -> Html msg
viewRibbon id settingsMsg disabledSettings =
    div [ class "budget-ribbon" ]
        [ a [ class "btn", href <| "/budget/" ++ String.fromInt id ] [ text "Detail" ]
        , button [ disabled True, class "btn" ] [ text "Statistics" ]
        , button [ class "btn", onClick settingsMsg, disabled disabledSettings ] [ text "Settings" ]
        ]


viewCurveDiagram : Budget -> Html msg
viewCurveDiagram budget =
    let
        content =
            if List.length budget.lines > 1 then
                viewCurve ( "Linear", Shape.linearCurve, Color.black ) budget

            else
                div [] [ text "Add more budget lines" ]
    in
    div [] [ h2 [] [ text "Finance movement" ], content ]


viewIncomeCategories : Budget -> Html msg
viewIncomeCategories budget =
    let
        list =
            getCategoriesCount budget (\b -> b.amount > 0)

        content =
            if List.length list > 0 then
                viewPie list

            else
                div [] [ text "Add more budget lines" ]
    in
    div [] [ h2 [] [ text "Income categories" ], content ]


viewExpenseCategories : Budget -> Html msg
viewExpenseCategories budget =
    let
        list =
            List.map (\( a, b ) -> ( a, b * -1 )) <| getCategoriesCount budget (\b -> b.amount < 0)

        content =
            if List.length list > 0 then
                viewPie list

            else
                div [] [ text "Add more budget lines" ]
    in
    div [] [ h2 [] [ text "Expense categories" ], content ]


view : BudgetWrapper -> ProfileDataWrapper -> (Int -> msg) -> List (Html msg)
view budgetData profileData settingsMsg =
    Api.defaultDataWrapperView profileData <|
        \profile ->
            Api.defaultDataWrapperView budgetData <|
                \b ->
                    [ div [ class "main-layout__inner" ]
                        [ h2 [] [ text b.name ]
                        , viewRibbon b.id (settingsMsg b.id) (not <| isAdmin b profile.id)
                        , div [ class "full-width" ] [ viewCurveDiagram b ]
                        , div [ class "full-width" ] [ viewIncomeCategories b ]
                        , div [ class "full-width" ] [ viewExpenseCategories b ]
                        ]
                    ]
