module Categories.Helpers exposing (..)

import String.Extra
import Categories.Types exposing (Category)

transformToName : Category -> String
transformToName c =
     String.Extra.toSentenceCase <| String.toLower <| String.replace "_" " " c.code
