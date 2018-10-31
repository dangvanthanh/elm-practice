module Main exposing (main)

import Browser
import Select2 exposing (..)


main =
    Browser.element { init = \() -> init, update = update, view = view, subscriptions = subscriptions }
