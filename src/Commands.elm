module Commands exposing (..)

import CsvDecode as Csv exposing ((|=))
import Date.Extra.Config.Config_ja_jp exposing (config)
import Date.Extra.Format as DateFormat
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Model exposing (City, Place, PlaceId)
import Msg exposing (Msg)
import RemoteData
import Todofuken
import Url


fetchPlaces : Cmd Msg
fetchPlaces =
    Http.get Url.fetchPlaceUrl placesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msg.OnFetchPlaces


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


fetchBuses : Model.Model -> Cmd Msg
fetchBuses model =
    Http.get (busUrlWithQuery model) busesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msg.OnFetchBuses


busUrlWithQuery : Model.Model -> String
busUrlWithQuery model =
    -- Url.fetchBusUrl
    Url.fetchBusUrl
        ++ "?"
        ++ "dep_date="
        ++ DateFormat.format config "%Y-%b-%-d" model.dateCheckIn
        ++ "&dep_pref="
        ++ (Todofuken.fromCode model.depPrefNum |> Maybe.map .name |> Maybe.withDefault "")
        ++ "&dest_pref="
        ++ (Todofuken.fromCode model.destPrefNum |> Maybe.map .name |> Maybe.withDefault "")


busesDecoder : Decode.Decoder (List Model.Bus)
busesDecoder =
    Decode.list busDecoder


busDecoder : Decode.Decoder Model.Bus
busDecoder =
    decode Model.Bus
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "company_name" Decode.string
        |> required "dep_date" Decode.string
        |> required "dep_city" Decode.string
        |> required "dep_time" Decode.string
        |> required "dest_city" Decode.string
        |> required "dest_time" Decode.string
        |> required "amount" Decode.int
        |> required "vacancy" Decode.string
        |> required "url" Decode.string


fetchCityList : Cmd Msg
fetchCityList =
    Http.send Msg.GetCityList <|
        Http.getString Url.fetchCityUrl


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
