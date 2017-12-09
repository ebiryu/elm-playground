module Model exposing (..)

import Animation exposing (px)
import Date exposing (Date)
import Draggable
import Element
import Mouse
import RemoteData exposing (WebData)
import Search.DatePickerUpdate as DatePicker
import Time exposing (Time)


type alias Model =
    { history : List (Maybe Route)
    , currentRoute : Maybe Route
    , drawerState : Bool
    , coordinate : LatLng
    , places : Places
    , ticket : Ticket
    , toggleSearch : Bool
    , citySearch : DepOrDest
    , citySearchString : String
    , citySearchResult : List City
    , depertureSelectedCity : City
    , destinationSelectedCity : City
    , cities : List City
    , errMsg : String
    , numOfPeople : NumOfPeople
    , numOfPeopleShow : Bool
    , dateNow : Date
    , dateCheckIn : Date
    , dateCheckOut : Date
    , datePickerModel : DatePicker.Model
    , datePickerShow : Bool
    , buses : WebData (List Bus)
    , drawerPosition : Animation.State
    , searchFromMapShow : Bool
    , depPrefNum : Int
    , destPrefNum : Int
    , hoveredPrefNum : Int
    , hoveredMap : Bool
    , positionOfMouse : Mouse.Position
    , mapPosition : MapPosition
    , mapZoom : Float
    , drag : Draggable.State String
    , positionOfMultiTouch : MultiTouchPosition
    , timeTouchedMap : Time
    , device : Element.Device
    }


type Ticket
    = OneWay
    | RoundTrip


type DepOrDest
    = Deperture
    | Destination


type alias LatLng =
    { latitude : Float
    , longitude : Float
    }


initLatLng : LatLng
initLatLng =
    { latitude = 48.2082, longitude = 16.3738 }


type alias PlaceId =
    String


type alias Place =
    { id : PlaceId
    , name : String
    , latitude : Float
    , longitude : Float
    }


type alias Places =
    WebData (List Place)


type Route
    = Home
    | Birds
    | Cats
    | Dogs
    | Map
    | History


type alias City =
    { id : CityId
    , prefecture : String
    , city : String
    , prefecture_kana : String
    , city_kana : String
    }


initCity : City
initCity =
    { id = ""
    , prefecture = ""
    , city = ""
    , prefecture_kana = ""
    , city_kana = ""
    }


type alias CityId =
    String


type alias NumOfPeople =
    { adult : Int
    , child : Int
    }


type alias Bus =
    { id : Int
    , name : String
    , companyName : String
    , depDate : String
    , depCity : String
    , depTime : String
    , destCity : String
    , destTime : String
    , amount : Int
    , vacancy : String
    , url : String
    }


type alias MapPosition =
    { x : Float
    , y : Float
    }


type alias MultiTouchPosition =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


initDrawerPosition : Animation.State
initDrawerPosition =
    Animation.style
        [ Animation.left (Animation.rem -16)
        ]
