module Main exposing (..)

import Animation
import Commands exposing (fetchPlaces)
import Date
import Date.Extra.Create exposing (dateFromFields)
import Draggable
import Element
import Model exposing (Model, Route(..))
import Mouse
import Msg exposing (Msg(..))
import Navigation
import RemoteData
import Search.DatePickerUpdate as DatePicker
import Task
import Time
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
    , drawerPosition = Model.initDrawerPosition
    , searchFromMapShow = False
    , depPrefNum = 13
    , destPrefNum = 27
    , hoveredPrefNum = 48
    , hoveredMap = False
    , positionOfMouse = { x = 0, y = 0 }
    , mapPosition = { x = 0, y = 0 }
    , mapZoom = 1
    , drag = Draggable.init
    , positionOfMultiTouch = { x = 0, y = 0 }
    , distanceOfMultiTouch = 0
    , timeTouchedMap = 0
    , toggleSingleFingerMove = False
    , singleFingerCoordinate = { x = 0, y = 0 }
    , fingers = Model.One
    , device = { width = 0, height = 0, phone = False, tablet = False, desktop = False, bigDesktop = False, portrait = False }
    }
        ! [ fetchPlaces

          -- , Commands.fetchCityList
          , Task.perform DateNow Date.now
          , Task.perform Resize Window.size
          ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate
            [ model.drawerPosition
            ]
        , Window.resizes Resize
        , Draggable.subscriptions DragMsg model.drag
        , Time.every (500 * Time.millisecond) Tick

        -- , case model.hoveredMap of
        --     True ->
        --         Mouse.moves MoveMouse
        --
        --     False ->
        --         Sub.none
        ]
