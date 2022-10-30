module Pages.Login exposing (page)

import Html exposing (Html)
import View exposing (View)


page : View msg
page =
    { title = "Pages.Login"
    , body = [ Html.text "/login" ]
    }
