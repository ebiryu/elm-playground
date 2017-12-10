module Msg exposing (..)

import Animation
import Date
import Dom
import Draggable
import Http
import Model exposing (Place)
import Mouse
import Navigation
import RemoteData exposing (WebData)
import Search.DatePickerUpdate as DatePicker
import Time exposing (Time)
import Touch
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
    | CloseResult
    | ToggleMap
    | ClickPrefecture Int
    | HoverPrefecture Int
    | HoverOutMap
    | ClickDeperture
    | ClickDestination
    | MoveMouse Mouse.Position
    | MapZoom Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | TimeTouchedMap Float
    | TimeLeftMap Int Touch.Event Float
    | Tick Time
    | SingleStart Int Touch.Event
    | SingleEnd Int Touch.Event
    | MultiStart Int Touch.Event
    | MultiMove Touch.Event
    | Zooming Touch.Event (List Touch.Coordinates)
    | Moving Touch.Event (List Touch.Coordinates)
    | Animate Animation.Msg
    | Resize Window.Size
    | InitAnimStyleOfMapDiv
