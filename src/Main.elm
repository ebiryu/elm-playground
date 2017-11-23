module Main exposing (..)

import Animation
import Commands exposing (fetchPlaces)
import Date
import Date.Extra.Create exposing (dateFromFields)
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Navigation
import RemoteData
import Search.DatePickerUpdate as DatePicker
import Task
import Update exposing (update)
import UrlParser as Url
import View exposing (view)
import Window


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- model


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { history = [ Url.parseHash Update.route location ]
    , currentRoute = Url.parseHash Update.route location
    , drawerState = False
    , coordinate = Model.initLatLng
    , places = RemoteData.Loading
    , ticket = Model.OneWay
    , toggleSearch = False
    , citySearch = Model.Deperture
    , citySearchString = ""
    , citySearchResult = []
    , depertureSelectedCity = Model.initCity
    , destinationSelectedCity = Model.initCity
    , cities = []
    , errMsg = ""
    , numOfPeople = { adult = 1, child = 0 }
    , numOfPeopleShow = False
    , dateNow = dateFromFields 2017 Date.Nov 17 0 0 0 0
    , dateCheckIn = dateFromFields 2017 Date.Nov 17 0 0 0 0
    , dateCheckOut = dateFromFields 2017 Date.Nov 18 0 0 0 0
    , datePickerModel = DatePicker.initDatePicker (dateFromFields 2017 Date.Nov 18 0 0 0 0)
    , datePickerShow = False
    , buses = RemoteData.NotAsked
    , searchConditionNumber = 0
    , searchConditionStyle =
        Model.initStyleOfConditions
    , drawerPosition = Model.initDrawerPosition
    , windowWidth = 0
    }
        ! [ fetchPlaces, Commands.fetchCityList, Task.perform DateNow Date.now, Task.perform WindowWidth Window.size ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate
            [ model.drawerPosition
            , model.searchConditionStyle.searchFormView
            , model.searchConditionStyle.howManyPeopleView
            , model.searchConditionStyle.datePickerView
            ]
        , Window.resizes WindowWidth
        ]
