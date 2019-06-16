module Budgets.Common exposing (isAdmin)

import Budgets.Types exposing (..)
import List.Extra
import Users.Types


isAdmin : Budget -> Int -> Bool
isAdmin b id =
    List.Extra.find (\a -> a.rel == Users.Types.Admin && a.user.id == id) b.users /= Nothing
