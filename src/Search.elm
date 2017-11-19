module Search exposing (..)

import Model exposing (City, Place, Places)
import RemoteData


runFilter : String -> Places -> List ( Model.PlaceId, String )
runFilter string placesData =
    case placesData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            [ ( "", "Loading" ) ]

        RemoteData.Success places ->
            if string == "" then
                []
            else
                filtering string places

        RemoteData.Failure error ->
            [ ( "", toString error ) ]


runFilter2 : String -> List City -> List City
runFilter2 string cities =
    if string == "" then
        []
    else
        filtering2 string cities


filtering : String -> List Place -> List ( Model.PlaceId, String )
filtering string places =
    List.map (\p -> ( p.id, p.name )) places
        |> List.filter (\( _, p ) -> String.contains string p)


filtering2 : String -> List City -> List City
filtering2 string cities =
    List.filter (\city -> String.contains string city.city || String.contains string city.city_kana) cities
