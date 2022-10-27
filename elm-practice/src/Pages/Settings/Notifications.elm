module Pages.Settings.Notifications exposing (page)

import Html exposing (Html)
import View exposing (View)


page : View msg
page =
    { title = "Pages.Settings.Notifications"
    , body = [ Html.text "/settings/notifications" ]
    }