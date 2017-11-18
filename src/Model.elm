module Model exposing (..)

import Animation exposing (px)
import Date exposing (Date)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import Search.DatePickerUpdate as DatePicker


type alias Model =
    { history : List (Maybe Route)
    , currentRoute : Maybe Route
    , drawerState : Bool
    , coordinate : LatLng
    , places : Places
    , toggleSearch : Bool
    , searchString : String
    , searchResult : List ( CityId, String )
    , selectedCityId : CityId
    , cities : List City
    , errMsg : String
    , numOfPeople : NumOfPeople
    , dateNow : Date
    , dateCheckIn : Date
    , dateCheckOut : Date
    , datePickerModel : DatePicker.Model
    , datePickerShow : Bool
    , searchConditionNumber : Int
    , searchConditionStyle : StyleOfConditions
    }


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


type alias City =
    { id : CityId
    , prefecture : String
    , city : String
    , prefecture_kana : String
    , city_kana : String
    }


type alias CityId =
    String


type alias NumOfPeople =
    { adult : Int
    , child : Int
    }


type alias StyleOfConditions =
    { searchFormView : Animation.State
    , howManyPeopleView : Animation.State
    , datePickerView : Animation.State
    }


initStyleOfConditions =
    { searchFormView =
        Animation.style
            [ Animation.translate (px 0.0) (px 0.0)
            , Animation.opacity 1.0
            ]
    , howManyPeopleView =
        Animation.style
            [ Animation.translate (px 50.0) (px 0.0)
            , Animation.opacity 0.0
            , Animation.display Animation.none
            ]
    , datePickerView =
        Animation.style
            [ Animation.translate (px 50.0) (px 0.0)
            , Animation.opacity 0.0
            , Animation.display Animation.none
            ]
    }
