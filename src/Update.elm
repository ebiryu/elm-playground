module Update exposing (..)

import Animation exposing (px)
import Commands
import Date.Extra.Duration as Duration
import Dict
import Dom
import Draggable
import Ease
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Search
import Search.DatePickerUpdate as DatePicker
import Task
import Time
import Touch
import UrlParser as Url


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Birds (Url.s "birds")
        , Url.map Cats (Url.s "cats")
        , Url.map Dogs (Url.s "dogs")
        , Url.map Map (Url.s "map")
        , Url.map History (Url.s "history")
        ]



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                nextRoute =
                    Url.parseHash route location
            in
            { model
                | currentRoute = nextRoute
                , history = nextRoute :: model.history
            }
                ! []

        ToggleDrawer bool ->
            if bool then
                { model | drawerState = False, drawerPosition = drawerSliceOut model.drawerPosition } ! []
            else
                { model | drawerState = True, drawerPosition = drawerSliceIn model.drawerPosition } ! []

        SetLatLng lat long ->
            { model | coordinate = { latitude = lat, longitude = long } } ! []

        SetLatitude lat ->
            { model | coordinate = { latitude = lat, longitude = model.coordinate.longitude } } ! []

        SetLongitude long ->
            { model | coordinate = { latitude = model.coordinate.latitude, longitude = long } } ! []

        OnFetchPlaces response ->
            { model | places = response } ! []

        OneWay ->
            { model | ticket = Model.OneWay } ! []

        RoundTrip ->
            { model | ticket = Model.RoundTrip } ! []

        ToggleSearch depOrDest ->
            { model | toggleSearch = not model.toggleSearch, citySearch = depOrDest, citySearchResult = [] }
                ! [ Task.attempt FocusOnInput (Dom.focus "search-place") ]

        FocusOnInput id ->
            ( model, Cmd.none )

        StartSearching string ->
            { model | citySearchString = string, citySearchResult = Search.runFilter2 string model.cities } ! []

        SelectCity city depDest ->
            case depDest of
                Model.Deperture ->
                    update (ToggleSearch model.citySearch) { model | depertureSelectedCity = city }

                Model.Destination ->
                    update (ToggleSearch model.citySearch) { model | destinationSelectedCity = city }

        GetCityList (Ok cities) ->
            { model | cities = Commands.runCsvDecoder cities } ! []

        GetCityList (Err err) ->
            ( { model | errMsg = toString err }, Cmd.none )

        SelectNumOfPeople adult child ->
            update ToggleNumOfPeople { model | numOfPeople = { adult = adult, child = child } }

        SetNumOfAdult adult ->
            { model
                | numOfPeople =
                    { adult = Result.withDefault 0 (String.toInt adult)
                    , child = model.numOfPeople.child
                    }
            }
                ! []

        SetNumOfChild child ->
            { model
                | numOfPeople =
                    { adult = model.numOfPeople.adult
                    , child = Result.withDefault 0 (String.toInt child)
                    }
            }
                ! []

        ToggleNumOfPeople ->
            { model | numOfPeopleShow = not model.numOfPeopleShow } ! []

        DateNow date ->
            { model
                | dateNow = date
                , dateCheckIn = date
                , dateCheckOut = Duration.add Duration.Day 1 date
                , datePickerModel = DatePicker.initDatePicker date
            }
                ! []

        DatePickerMsg msg check ->
            let
                newDate =
                    DatePicker.update msg model.datePickerModel
            in
            case msg of
                DatePicker.ClickDay int ->
                    case check of
                        DatePicker.CheckIn ->
                            update (ToggleDatePicker model.datePickerModel.check) { model | datePickerModel = newDate, dateCheckIn = newDate.date }

                        DatePicker.CheckOut ->
                            update (ToggleDatePicker model.datePickerModel.check) { model | datePickerModel = newDate, dateCheckOut = newDate.date }

                _ ->
                    ( { model | datePickerModel = newDate }, Cmd.none )

        ToggleDatePicker check ->
            let
                oldModel =
                    model.datePickerModel

                newModel =
                    case check of
                        DatePicker.CheckIn ->
                            { oldModel | date = model.dateCheckIn, check = check }

                        DatePicker.CheckOut ->
                            { oldModel | date = model.dateCheckOut, check = check }
            in
            { model | datePickerShow = not model.datePickerShow, datePickerModel = newModel } ! []

        OnFetchBuses response ->
            { model | buses = response } ! []

        SubmitSearch ->
            ( model, Commands.fetchBuses model )

        Animate animMsg ->
            { model
                | drawerPosition = Animation.update animMsg model.drawerPosition
            }
                ! []

        ToggleMap ->
            { model | searchFromMapShow = not model.searchFromMapShow } ! []

        ClickPrefecture prefNum ->
            case model.citySearch of
                Model.Deperture ->
                    { model | depPrefNum = prefNum + 1, citySearch = Model.Destination } ! []

                Model.Destination ->
                    { model | destPrefNum = prefNum + 1, citySearch = Model.Deperture } ! []

        ClickDeperture ->
            { model | citySearch = Model.Deperture } ! []

        ClickDestination ->
            { model | citySearch = Model.Destination } ! []

        MoveMouse position ->
            { model | positionOfMouse = position } ! []

        HoverPrefecture prefNum ->
            { model | hoveredPrefNum = prefNum + 1, hoveredMap = True } ! []

        HoverOutMap ->
            { model | hoveredMap = False } ! []

        MapZoom factor ->
            let
                newZoom =
                    (factor * 0.005)
                        |> (-) model.mapZoom
                        |> clamp 1 2
            in
            ( { model | mapZoom = newZoom }, Cmd.none )

        OnDragBy ( dx, dy ) ->
            let
                pos =
                    model.mapPosition

                newX =
                    (pos.x + dx) |> clamp -300 300

                newY =
                    (pos.y + dy) |> clamp -250 250
            in
            { model | mapPosition = { x = newX, y = newY } } ! []

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        TimeTouchedMap time ->
            { model | timeTouchedMap = Debug.log "time" time } ! []

        SingleStart int event ->
            model ! [ Task.perform TimeTouchedMap Time.now ]

        SingleEnd int event ->
            model ! [ Task.perform (TimeLeftMap int event) Time.now ]

        TimeLeftMap int event time ->
            if time - model.timeTouchedMap < 300 then
                if int <= 46 && int >= 0 then
                    update (ClickPrefecture int) model
                else
                    ( model, Cmd.none )
            else
                ( model, Cmd.none )

        Tick time ->
            model ! []

        MultiStart int event ->
            let
                numberOfFinger =
                    List.length <| Dict.values <| Touch.touches event

                eventPositions =
                    Dict.values <| Touch.touches event

                oldPositions =
                    model.positionOfMultiTouch

                positions =
                    case eventPositions of
                        [ x1, x2 ] ->
                            { x1 = x1.clientX, y1 = x1.clientY, x2 = x2.clientX, y2 = x2.clientY }

                        _ ->
                            oldPositions
            in
            update (SingleStart int event) { model | positionOfMultiTouch = positions }

        MultiMove event ->
            let
                oldPositions =
                    model.positionOfMultiTouch

                oldDistance =
                    sqrt ((oldPositions.x2 - oldPositions.x1) ^ 2 + (oldPositions.y2 - oldPositions.y1) ^ 2)

                eventPositions =
                    Dict.values <| Touch.changedTouches event

                positions =
                    case eventPositions of
                        [ x1, x2 ] ->
                            { x1 = x1.clientX, y1 = x1.clientY, x2 = x2.clientX, y2 = x2.clientY }

                        _ ->
                            oldPositions

                distance =
                    sqrt ((positions.x2 - positions.x1) ^ 2 + (positions.y2 - positions.y1) ^ 2)

                newZoom =
                    ((oldDistance - distance) * 0.005)
                        |> (-) model.mapZoom
                        |> clamp 1 2

                pos =
                    model.mapPosition

                ( oldCx, oldCy ) =
                    ( (oldPositions.x1 + oldPositions.x2) / 2, (oldPositions.y1 + oldPositions.y2) / 2 )

                ( cx, cy ) =
                    ( (positions.x1 + positions.x2) / 2, (positions.y1 + positions.y2) / 2 )

                ( dx, dy ) =
                    ( cx - oldCx, cy - oldCy )

                newX =
                    (pos.x + dx) |> clamp -300 300

                newY =
                    (pos.y + dy) |> clamp -250 250
            in
            { model | positionOfMultiTouch = positions, mapZoom = newZoom, mapPosition = { x = newX, y = newY } } ! []

        WindowWidth size ->
            { model | windowWidth = size.width } ! []


easing : Animation.Interpolation
easing =
    let
        params =
            { duration = 0.2 * Time.second
            , ease = Ease.outCubic
            }
    in
    Animation.easing params


drawerSliceIn : Animation.State -> Animation.State
drawerSliceIn style =
    Animation.queue [ Animation.toWith easing [ Animation.left (Animation.rem 0) ] ] style


drawerSliceOut : Animation.State -> Animation.State
drawerSliceOut style =
    Animation.queue [ Animation.set [ Animation.left (Animation.rem -16) ] ] style


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy
