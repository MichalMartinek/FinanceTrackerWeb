module Api exposing (..)

import Http

apiUrl : String
apiUrl =
    "http://localhost:8000"


type Status
    = Loading
    | Error Http.Error
    | Success
