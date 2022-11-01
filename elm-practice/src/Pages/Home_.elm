module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.PokemonList
import Html exposing (Html)
import Http
import Layout exposing (Layout)
import Page exposing (Page)
import View exposing (View)


layout : Layout
layout =
    Layout.Sidebar


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { pokemonData : Api.Data (List Pokemon)
    }


type alias Pokemon =
    { name : String
    }


init : ( Model, Cmd Msg )
init =
    ( { pokemonData = Api.Loading }
    , Api.PokemonList.getFirst150
        { onResponse = PokemonApiResponded
        }
    )



-- UPDATE


type Msg
    = PokemonApiResponded (Result Http.Error (List Pokemon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PokemonApiResponded (Ok listOfPokemon) ->
            ( { model | pokemonData = Api.Success listOfPokemon }
            , Cmd.none
            )

        PokemonApiResponded (Err httpError) ->
            ( { model | pokemonData = Api.Failure httpError }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body =
        case model.pokemonData of
            Api.Loading ->
                [ Html.text "Loading" ]

            Api.Success listOfPokemon ->
                let
                    count : Int
                    count =
                        List.length listOfPokemon
                in
                [ Html.text ("Fetch " ++ String.fromInt count ++ " pokemon!") ]

            Api.Failure httpError ->
                [ Html.text "Something went wrong..." ]
    }
