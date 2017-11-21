module Commands exposing (..)

import CsvDecode as Csv exposing ((|=))
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Model exposing (City, Place, PlaceId)
import Msg exposing (Msg)
import RemoteData


fetchPlaces : Cmd Msg
fetchPlaces =
    Http.get fetchPlaceUrl placesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msg.OnFetchPlaces


fetchPlaceUrl : String
fetchPlaceUrl =
    "http://localhost:4000/places"


placesDecoder : Decode.Decoder (List Place)
placesDecoder =
    Decode.list placeDecoder


placeDecoder : Decode.Decoder Place
placeDecoder =
    decode Place
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "latitude" Decode.float
        |> required "longitude" Decode.float


fetchBuses : Cmd Msg
fetchBuses =
    Http.get "http://localhost:4000/buses" busesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msg.OnFetchBuses


busesDecoder : Decode.Decoder (List Model.Bus)
busesDecoder =
    Decode.list busDecoder


busDecoder =
    decode Model.Bus
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "companyName" Decode.string
        |> required "depPrefecture" Decode.string
        |> required "depDate" Decode.string
        |> required "destPrefecture" Decode.string
        |> required "destDate" Decode.string
        |> required "amount" Decode.int


fetchCityList : Cmd Msg
fetchCityList =
    Http.send Msg.GetCityList <|
        Http.getString "http://localhost:3000/fromGov.csv"


runCsvDecoder : String -> List City
runCsvDecoder string =
    Csv.run csvDecoder string
        |> Result.withDefault []


csvDecoder : Csv.Decoder City
csvDecoder =
    Csv.succeed City
        |= Csv.field "id"
        |= Csv.field "prefecture"
        |= Csv.string (Csv.field "city")
        |= Csv.field "prefecture_kana"
        |= Csv.string (Csv.field "city_kana")
