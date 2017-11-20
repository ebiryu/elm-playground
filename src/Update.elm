module Update exposing (..)

import Animation exposing (px)
import Commands
import Date.Extra.Duration as Duration
import Dom
import Ease
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Navigation
import RemoteData
import Search
import Search.DatePickerUpdate as DatePicker
import Task
import Time
import UrlParser as Url
import View exposing (view)


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

        ToggleDrawer ->
            { model | drawerState = not model.drawerState } ! []

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
            { model | toggleSearch = not model.toggleSearch, citySearch = depOrDest, citySearchResult = [], searchConditionStyle = Model.initStyleOfConditions }
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
            ( model, Commands.fetchBuses )

        NextCondition1 ->
            let
                oldStyle =
                    model.searchConditionStyle

                newStyle =
                    { oldStyle
                        | searchFormView = fadeOutNext oldStyle.searchFormView
                        , howManyPeopleView = fadeIn oldStyle.howManyPeopleView
                    }
            in
            ( { model | searchConditionStyle = newStyle }, Cmd.none )

        BeforeCondition1 ->
            let
                oldStyle =
                    model.searchConditionStyle

                newStyle =
                    { oldStyle
                        | searchFormView = fadeIn oldStyle.searchFormView
                        , howManyPeopleView = fadeOutBefore oldStyle.howManyPeopleView
                    }
            in
            ( { model | searchConditionStyle = newStyle }, Cmd.none )

        NextCondition2 ->
            let
                oldStyle =
                    model.searchConditionStyle

                newStyle =
                    { oldStyle
                        | howManyPeopleView = fadeOutNext oldStyle.howManyPeopleView
                        , datePickerView = fadeIn oldStyle.datePickerView
                    }
            in
            ( { model | searchConditionStyle = newStyle }, Cmd.none )

        BeforeCondition2 ->
            let
                oldStyle =
                    model.searchConditionStyle

                newStyle =
                    { oldStyle
                        | howManyPeopleView = fadeIn oldStyle.howManyPeopleView
                        , datePickerView = fadeOutBefore oldStyle.datePickerView
                    }
            in
            ( { model | searchConditionStyle = newStyle }, Cmd.none )

        Animate animMsg ->
            { model
                | searchConditionStyle =
                    { searchFormView = Animation.update animMsg model.searchConditionStyle.searchFormView
                    , howManyPeopleView = Animation.update animMsg model.searchConditionStyle.howManyPeopleView
                    , datePickerView = Animation.update animMsg model.searchConditionStyle.datePickerView
                    }
            }
                ! []

        WindowWidth size ->
            { model | windowWidth = size.width } ! []


easing =
    let
        params =
            { duration = 0.15 * Time.second
            , ease = Ease.outQuart
            }
    in
    Animation.easing params


fadeOutNext view =
    Animation.queue
        [ Animation.toWith easing
            [ Animation.translate (px 0.0) (px 0.0), Animation.opacity 0.0 ]
        ]
        view
        |> Animation.queue [ Animation.set [ Animation.display Animation.none ] ]


fadeOutBefore view =
    Animation.queue
        [ Animation.toWith easing
            [ Animation.translate (px 50.0) (px 0.0), Animation.opacity 0.0 ]
        ]
        view
        |> Animation.queue [ Animation.set [ Animation.display Animation.none ] ]


fadeIn view =
    Animation.queue [ Animation.wait (Time.second * 0.1) ] view
        |> Animation.queue [ Animation.set [ Animation.display Animation.block ] ]
        |> Animation.queue
            [ Animation.toWith easing
                [ Animation.translate (px 0.0) (px 0.0)
                , Animation.opacity 1.0
                ]
            ]
