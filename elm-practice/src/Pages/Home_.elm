module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.PokemonList
import Html exposing (Html)
import Html.Attributes exposing (alt, class, src)
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
    { title = "Pokemon"
    , body =
        [ Html.div [ class "hero is-danger py-6 has-text-centered" ]
            [ Html.h1 [ class "title is-1" ] [ Html.text "Pokemon" ]
            , Html.h2 [ class "subtitle is-4" ] [ Html.text "Gotta fetch em all!" ]
            ]
        , case model.pokemonData of
            Api.Loading ->
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text "Loading..." ]

            Api.Success pokemon ->
                viewPokemonList pokemon

            Api.Failure httpError ->
                Html.div [ class "has-text-centered p-6" ]
                    [ Html.text "Something went wrong..." ]
        ]
    }


viewPokemonList : List Pokemon -> Html Msg
viewPokemonList listOfPokemon =
    Html.div [ class "container py-6 px-5" ]
        [ Html.div [ class "columns is-multiline" ]
            (List.indexedMap viewPokemon listOfPokemon)
        ]


viewPokemon : Int -> Pokemon -> Html Msg
viewPokemon index pokemon =
    let
        pokedexNumber : Int
        pokedexNumber =
            index + 1

        pokemonImageUrl : String
        pokemonImageUrl =
            "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/"
                ++ String.fromInt pokedexNumber
                ++ ".png"
    in
    Html.div [ class "column is-4-desktop is-6-tablet" ]
        [ Html.div [ class "card" ]
            [ Html.div [ class "card-content" ]
                [ Html.div [ class "media" ]
                    [ Html.div [ class "media-left" ]
                        [ Html.figure [ class "image is-64x64" ]
                            [ Html.img [ src pokemonImageUrl, alt pokemon.name ] []
                            ]
                        ]
                    , Html.div [ class "media-content" ]
                        [ Html.p [ class "title is-4" ] [ Html.text pokemon.name ]
                        , Html.p [ class "subtitle is-6" ] [ Html.text ("No. " ++ String.fromInt pokedexNumber) ]
                        ]
                    ]
                ]
            ]
        ]
