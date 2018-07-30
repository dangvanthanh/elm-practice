port module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (id)
import Dict exposing (Dict)


type alias Model =
    { selection : Maybe String }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, output (Dict.toList options) )


type Msg
    = Select String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select val ->
            let
                option : Maybe String
                option =
                    Dict.get val options
            in
                ( { model | selection = option }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text (toString model)
        , div [ id "select2-container" ] []
        ]


port output : List ( String, String ) -> Cmd msg


port input : (String -> msg) -> Sub msg


options : Dict String String
options =
    Dict.fromList
        [ ( "US", "United States" )
        , ( "UK", "United Kingdom" )
        , ( "VN", "Vietnam" )
        , ( "CA", "Cannada" )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    input Select
