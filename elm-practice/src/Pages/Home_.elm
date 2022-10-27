module Pages.Home_ exposing (page)

import Html
import Layout exposing (Layout)
import View exposing (View)

layout: Layout
layout =
    Layout.Sidebar


page : View msg
page =
    { title = "Elm Practice"
    , body = [ Html.text "Hello, world!" ]
    }
