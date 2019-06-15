module BudgetLines.Types exposing (..)

import Categories.Types exposing (Category)
import Time

type alias BudgetLine =
    { id : Int
    , description : String
    , amount : Float
    , category : Category
    , date_created : Time.Posix
    }

type alias BudgetLineForm =
    { description : String
    , amount : Float
    , category : String
    }

