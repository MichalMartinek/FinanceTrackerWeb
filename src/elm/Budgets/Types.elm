module Budgets.Types exposing (..)


import BudgetLines.Types as BudgetLinesTypes exposing (BudgetLine)
import Users.Types exposing (UserWithRole)
import Time

type alias Budget =
    { id : Int
    , currency : String
    , name : String
    , date_created : Time.Posix
    , date_updated : Time.Posix
    , lines : List BudgetLine
    , users : List UserWithRole
    }
