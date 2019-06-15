module Categories.Types exposing (Category, CategoriesResult)

type alias Category =
    { code : String
    }



type alias CategoriesResult =
    {
        results: (List Category)
    }