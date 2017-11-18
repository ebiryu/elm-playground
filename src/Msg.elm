module Msg exposing (..)

import Animation
import Date
import Dom
import Http
import Json.Encode exposing (Value)
import Model exposing (Place)
import Navigation
import RemoteData exposing (WebData)
import Search.DatePickerUpdate as DatePicker


type Msg
    = UrlChange Navigation.Location
    | ToggleDrawer
    | SetLatLng Float Float
    | SetLatitude Float
    | SetLongitude Float
    | OnFetchPlaces (WebData (List Place))
    | ToggleSearch
    | StartSearching String
    | FocusOnInput (Result Dom.Error ())
    | SelectCityId String
    | GetCityList (Result Http.Error String)
    | SelectNumOfPeople Int Int
    | SetNumOfAdult String
    | SetNumOfChild String
    | DateNow Date.Date
    | DatePickerMsg DatePicker.Msg DatePicker.Check
    | ToggleDatePicker DatePicker.Check
    | NextCondition1
    | BeforeCondition1
    | NextCondition2
    | BeforeCondition2
    | Animate Animation.Msg
