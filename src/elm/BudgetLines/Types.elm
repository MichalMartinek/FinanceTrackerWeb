module BudgetLines.Types exposing (..)


type alias Category =
    { code : String
    }


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

