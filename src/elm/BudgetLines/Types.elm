module BudgetLines.Types exposing (..)

import Categories.Types exposing (Category)

type alias BudgetLine =
    { id : Int
    , description : String
    , amount : Float
    , category : Category
    }

type alias BudgetLineForm =
    { description : String
    , amount : Float
    , category : String
    }

