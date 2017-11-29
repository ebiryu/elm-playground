module View exposing (..)

-- import MapboxAccessToken exposing (mapboxToken)

import Animation
import Date
import Date.Extra.Config.Config_ja_jp exposing (config)
import Date.Extra.Format as DateFormat
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import RemoteData
import Search.DatePicker
import Search.DatePickerUpdate exposing (Check(..))
import Search.View as Search
import Style as Style


view : Model -> Html Msg
view model =
    div []
        [ header_ model
        , mainView model
        , if model.toggleSearch then
            Search.searchFormView model
          else if model.datePickerShow then
            Search.DatePicker.view model.datePickerModel
          else if model.numOfPeopleShow then
            Search.howManyPeopleView model
          else
            text ""
        ]


header_ : Model -> Html Msg
header_ model =
    -- header [ style Style.header ]
    header [ class "flex items-center bg-navy h3 shadow-2" ]
        [ a
            (case model.drawerState of
                True ->
                    []

                False ->
                    [ onClick (ToggleDrawer model.drawerState) ]
            )
            [ i
                [ class "white ma2 material-icons md-36 pointer"
                ]
                [ text "menu" ]
            ]
        , a [ class "link white f2", href "#" ] [ text "elm-sample-spa" ]
        , div [ class "ml-auto mr2 flex" ]
            (List.map viewLinkTab [ "birds", "cats", "dogs", "map", "history" ])
        , case model.drawerState of
            True ->
                drawerView model

            False ->
                span [] []
        ]


mainView : Model -> Html Msg
mainView model =
    div []
        [ case model.currentRoute of
            Just Home ->
                if model.windowWidth < 550 then
                    homeMinimalView model
                else
                    homeView model

            Just Birds ->
                animalView "birds" "have wings and a beak"

            Just Cats ->
                animalView "cats" ""

            Just Dogs ->
                animalView "dogs" ""

            Just Map ->
                mapView model

            Just History ->
                historyView model

            Nothing ->
                notFoundView
        ]


homeView : Model -> Html Msg
homeView model =
    div [ style Style.boxed ]
        [ div
            [ class "center"
            , style [ ( "width", "500px" ) ]
            ]
            [ homeMinimalView model ]
        ]


homeMinimalView : Model -> Html Msg
homeMinimalView model =
    div []
        [ h1 [ class "center w-90 f3" ] [ text "場所と日付を選択" ]
        , div [ class "center w-90" ]
            [ div [ class "ml2" ]
                [ div
                    [ class "dib mh2 pv2 ph3 bt bl br bw1 br--top br2 pointer"
                    , classList [ ( "bg-navy b--navy white", model.ticket == Model.OneWay ) ]
                    , onClick OneWay
                    ]
                    [ text "片道" ]
                , div
                    [ class "dib mh2 pv2 ph3 bt bl br bw1 br--top br2 pointer"
                    , classList [ ( "bg-navy b--navy white", model.ticket == Model.RoundTrip ) ]
                    , onClick RoundTrip
                    ]
                    [ text "往復" ]
                ]
            , div [ class "pa2 br2 ba bw1 b--navy bg-navy shadow-2" ]
                [ searchPlaceButton "from:" model.depertureSelectedCity Model.Deperture
                , searchPlaceButton "to:" model.destinationSelectedCity Model.Destination
                , div [ class "flex justify-between" ] <|
                    List.concat
                        [ case model.ticket of
                            Model.OneWay ->
                                [ div
                                    [ class "dark-silver bg-nearwhite br2 mv2 pa2 w-40 bg-near-white pointer shadow-1"
                                    , style [ ( "width", "54%" ) ]
                                    , onClick (ToggleDatePicker CheckIn)
                                    ]
                                    [ text (DateFormat.format config "%b/%-d (%a)" model.dateCheckIn) ]
                                ]

                            Model.RoundTrip ->
                                [ div
                                    [ class "dark-silver bg-nearwhite br2 mv2 pa2 w-40 bg-near-white pointer shadow-1"
                                    , style [ ( "width", "27%" ) ]
                                    , onClick (ToggleDatePicker CheckIn)
                                    ]
                                    [ text (DateFormat.format config "%b/%-d (%a)" model.dateCheckIn) ]
                                , div
                                    [ class "dark-silver bg-nearwhite br2 mv2 pa2 w-40 bg-near-white pointer shadow-1"
                                    , style [ ( "width", "27%" ) ]
                                    , onClick (ToggleDatePicker CheckOut)
                                    ]
                                    [ text (DateFormat.format config "%b/%-d (%a)" model.dateCheckOut) ]
                                ]
                        , [ div
                                [ class "dark-silver bg-nearwhite br2 mv2 pa2 w-40 bg-near-white pointer shadow-1"
                                , style [ ( "width", "42%" ) ]
                                , onClick ToggleNumOfPeople
                                ]
                                [ span [ class "dib" ]
                                    [ text "大人: "
                                    , text <| toString model.numOfPeople.adult
                                    , text "人, "
                                    ]
                                , span [ class "dib" ]
                                    [ text "子供: "
                                    , text <| toString model.numOfPeople.child
                                    , text "人"
                                    ]
                                ]
                          ]
                        ]
                , div [ class "ml-auto mv2 pa2 tc w3 bg-near-white br2 pointer", onClick SubmitSearch ] [ text "検索" ]
                ]
            , searchResultBusList model
            ]
        ]


searchPlaceButton : String -> Model.City -> Model.DepOrDest -> Html Msg
searchPlaceButton string city depDest =
    div [ class "silver bg-nearwhite br2 mv2 pa2 w-100 bg-near-white pointer shadow-1", onClick (ToggleSearch depDest) ]
        [ span [ class "f5" ]
            [ text string
            , text " "
            , text city.city
            ]
        ]


searchResultBusList : Model -> Html Msg
searchResultBusList model =
    case model.buses of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ text "Now loading ..." ]

        RemoteData.Success buses ->
            let
                list bus =
                    div [ class "ba b--silver br3 pa3 mv3 hover-bg-black-10" ]
                        [ div [ class "f3" ] [ text bus.companyName ]
                        , div [] [ text bus.name ]
                        , div [] [ text <| "￥" ++ toString bus.amount, text <| " 空席 :" ++ bus.vacancy ]
                        , div [ class "mt2" ]
                            [ div [ class "dib" ]
                                [ div [] [ text bus.depCity ]
                                , div []
                                    [ text <|
                                        DateFormat.formatOffset config -540 "%b/%-d (%a) %k:%M" <|
                                            Result.withDefault model.dateCheckIn <|
                                                Date.fromString bus.depTime
                                    ]
                                ]
                            , div [ class "dib" ] [ i [ class "material-icons" ] [ text "navigate_next" ] ]
                            , div [ class "dib" ]
                                [ div [] [ text bus.destCity ]
                                , div []
                                    [ text <|
                                        DateFormat.formatOffset config -540 "%b/%-d (%a) %k:%M" <|
                                            Result.withDefault model.dateCheckIn <|
                                                Date.fromString bus.destTime
                                    ]
                                ]
                            ]
                        , div [] (List.map showCityAndTime (organizeCities bus.depCity))
                        ]
            in
            div [ class "mt3 navy" ] (List.map list buses)

        RemoteData.Failure error ->
            div [] []


organizeCities : String -> List ( String, String )
organizeCities str =
    str
        |> String.dropLeft 2
        |> String.dropRight 2
        |> String.split "],["
        |> List.map
            (\s ->
                ( Maybe.withDefault "" <| List.head <| String.split "," s
                , Maybe.withDefault "" <| List.head <| Maybe.withDefault [] <| List.tail <| String.split "," s
                )
            )


showCityAndTime : ( String, String ) -> Html msg
showCityAndTime ( city, time ) =
    div [ class "dib" ] [ text city, br [] [], text time ]


historyView : Model -> Html Msg
historyView model =
    div []
        [ h1 [] [ text "History" ]
        , ul [] (List.map viewRoute (List.reverse model.history))
        ]


mapView : Model -> Html Msg
mapView model =
    div []
        [ case model.places of
            RemoteData.NotAsked ->
                text ""

            RemoteData.Loading ->
                text "Loading"

            RemoteData.Success places ->
                listPlaces places

            RemoteData.Failure error ->
                text (toString error)
        ]


listPlaces : List Model.Place -> Html Msg
listPlaces places =
    div [ class "list p10" ] (List.map placeLi places)


placeLi : Model.Place -> Html Msg
placeLi place =
    div []
        [ article
            [ class "center mw5 mw6-ns br3 hidden ba b--black-10 mv4"
            , onMouseOver (SetLatLng place.latitude place.longitude)
            ]
            [ div [ class "f4 bg-near-white br3 br--top black-60 mv0 pv2 ph3" ] [ text place.name ]
            , div [ class "pa3 bt b--black-10" ]
                [ pInList <| toString place.latitude
                , pInList <| toString place.longitude
                ]
            ]
        ]


pInList : String -> Html msg
pInList string =
    p [ class "f6 f5-ns lh-copy measure" ] [ text string ]


viewRoute : Maybe Route -> Html msg
viewRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            li [] [ text "Invalid URL" ]

        Just route ->
            li [] [ text (toString route) ]


viewLinkTab : String -> Html msg
viewLinkTab name =
    span [ class "dim h2 w3 dn dib-l" ]
        [ a [ href ("#" ++ name), class "link flex justify-center items-center h2 mr1 ml1 white ba br3 bw1 b--white" ]
            [ span [] [ text name ] ]
        ]


drawerView : Model -> Html Msg
drawerView model =
    div [ class "absolute absolute--fill bg-black-50 z2" ]
        [ div [ class "absolute absolute--fill", onClick (ToggleDrawer model.drawerState) ] []
        , div
            (List.concat
                [ Animation.render model.drawerPosition
                , [ class "absolute left-0 top-0 w5 vh-100 bg-near-white z3" ]
                ]
            )
            [ ul [] (List.map viewLink [ "birds", "cats", "dogs", "map", "history" ])
            ]
        ]


viewLink : String -> Html msg
viewLink name =
    li [] [ a [ href ("#" ++ name) ] [ text name ] ]


animalView : String -> String -> Html msg
animalView name description =
    let
        animalDescription =
            name ++ " " ++ description
    in
    div []
        [ p [] [ a [ href "#" ] [ text "Back to index" ] ]
        , h1 [] [ text animalDescription ]
        ]


notFoundView : Html msg
notFoundView =
    div []
        [ h1 [] [ text "Not found :(" ]
        ]
