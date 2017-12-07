module Msg exposing (..)

import Animation
import Date
import Dom
import Http
import Model exposing (Place)
import Mouse
import Navigation
import RemoteData exposing (WebData)
import Search.DatePickerUpdate as DatePicker
import Window exposing (Size)


type Msg
    = UrlChange Navigation.Location
    | ToggleDrawer Bool
    | SetLatLng Float Float
    | SetLatitude Float
    | SetLongitude Float
    | OnFetchPlaces (WebData (List Place))
    | OneWay
    | RoundTrip
    | ToggleSearch Model.DepOrDest
    | StartSearching String
    | FocusOnInput (Result Dom.Error ())
    | SelectCity Model.City Model.DepOrDest
    | GetCityList (Result Http.Error String)
    | SelectNumOfPeople Int Int
    | SetNumOfAdult String
    | SetNumOfChild String
    | ToggleNumOfPeople
    | DateNow Date.Date
    | DatePickerMsg DatePicker.Msg DatePicker.Check
    | ToggleDatePicker DatePicker.Check
    | OnFetchBuses (WebData (List Model.Bus))
    | SubmitSearch
    | ToggleMap
    | ClickPrefecture Int
    | HoverPrefecture Int
    | HoverOutMap
    | ClickDeperture
    | ClickDestination
    | MoveMouse Mouse.Position
    | Animate Animation.Msg
    | WindowWidth Size
