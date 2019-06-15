module Users.Types exposing (BudgetData, BudgetWithRoleData, ProfileData, ProfileDataWrapper, Role(..), User, UserWithRole)

import Api
import Time


type alias BudgetData =
    { id : Int
    , currency : String
    , name : String
    , date_created : Time.Posix
    , date_updated : Time.Posix
    }


type Role
    = Admin
    | Viewer


type alias BudgetWithRoleData =
    { budget : BudgetData
    , rel : Role
    }


type alias ProfileData =
    { id : Int
    , username : String
    , budgets : List BudgetWithRoleData
    }


type alias ProfileDataWrapper =
    Api.DataWrapper ProfileData


type alias User =
    { id : Int
    , username : String
    }


type alias UserWithRole =
    { user : User
    , rel : Role
    , id : Int
    }
