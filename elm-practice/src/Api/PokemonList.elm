module Api.PokemonList exposing (Pokemon, getFirst150)

import Http
import Json.Decode


getFirst150 :
    { onResponse : Result Http.Error (List Pokemon) -> msg
    }
    -> Cmd msg
getFirst150 options =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon?limit=150"
        , expect = Http.expectJson options.onResponse decoder
        }


decodor : Json.Decode.Decoder (List Pokemon)
decodor =
    Json.Decode.field "results" (Json.Decode.list pokemonDecoder)


type alias Pokemon =
    { name : String }


pokemonDecoder : Json.Decode.Decoder Pokemon
pokemonDecoder =
    Json.Decode.map Pokemon
        (Json.Decode.field "name" Json.Decode.string)
