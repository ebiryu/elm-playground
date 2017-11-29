module Update exposing (..)

import Animation exposing (px)
import Commands
import Date.Extra.Duration as Duration
import Dom
import Ease
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Search
import Search.DatePickerUpdate as DatePicker
import Task
import Time
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
