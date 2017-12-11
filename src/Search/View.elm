module Search.View exposing (howManyPeopleView, searchFormView, searchFromMapView, sfmvHeaderHeight, sfmvSearchHeight)

import Animation
import Color exposing (rgb)
import Date
import Date.Extra.Config.Config_ja_jp exposing (config)
import Date.Extra.Format as DateFormat
import Element exposing (column, el, row)
import Element.Attributes as EA exposing (alignBottom, center, spacing, spread)
import Element.Events as EE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import RemoteData
import Search.DatePicker
import Search.DatePickerUpdate exposing (Check(CheckIn, CheckOut))
import Search.Map as Map
import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Todofuken


type MyStyle
    = None
    | Header
    | Deperture
    | Destination
    | Date
    | Map
    | Submit
    | BoxTitle
    | BoxMain
    | SearchBox


styleSheet : Model -> Style.StyleSheet MyStyle a
styleSheet model =
    Style.styleSheet
        [ Style.style None []
        , Style.style Header [ Border.bottom 1 ]
        , Style.style Deperture
            [ Style.cursor "pointer"
            , Color.border (rgb 48 63 159)
            , if model.citySearch == Model.Deperture then
                Border.bottom 6
              else
                Border.bottom 3
            ]
        , Style.style Destination
            [ Style.cursor "pointer"
            , Color.border (rgb 255 87 34)
            , if model.citySearch == Model.Destination then
                Border.bottom 6
              else
                Border.bottom 3
            ]
        , Style.style Date
            [ Border.bottom 3, Style.cursor "pointer", Color.border borderColor ]
        , Style.style Map [ Shadow.inset { offset = ( 0, 0 ), size = 2, blur = 10, color = rgb 150 150 150 } ]
        , Style.style Submit
            [ Style.cursor "pointer"
            , Color.background (rgb 255 87 34)
            , Color.text (rgb 240 240 240)
            , Border.rounded 24
            , Shadow.box { offset = ( 1, 1 ), size = 1, blur = 5, color = rgb 150 150 150 }
            ]
        , Style.style BoxTitle [ Font.size 14 ]
        , Style.style BoxMain [ Font.size 24 ]
        , Style.style SearchBox [ Shadow.box { offset = ( 0, 1 ), size = 1, blur = 5, color = rgb 150 150 150 } ]
        ]


borderColor : Color.Color
borderColor =
    rgb 230 230 230


sfmvHeaderHeight : Float
sfmvHeaderHeight =
    48


sfmvSearchHeight : Float
sfmvSearchHeight =
    210


renderAnim : Animation.State -> List (Element.Attribute variation Msg) -> List (Element.Attribute variation Msg)
renderAnim animStyle otherAttrs =
    (List.map EA.toAttr <| Animation.render animStyle) ++ otherAttrs


searchFromMapView : Model -> Html Msg
searchFromMapView model =
    div [ class "absolute absolute--fill bg-white fixed z2" ]
        [ Element.layout (styleSheet model) <|
            column None
                [ EA.width (EA.percent 100), EA.height (EA.px <| toFloat model.device.height) ]
                [ el Header [ EA.height (EA.px sfmvHeaderHeight) ] <|
                    Element.html (i [ class "material-icons md-48 pointer", onClick ToggleMap ] [ text "navigate_before" ])
                , el Map
                    (renderAnim model.animStyleOfMapDiv [])
                    (Element.html (Map.maps model))
                , el SearchBox
                    [ EA.height (EA.px sfmvSearchHeight), EA.width (EA.percent 100) ]
                    (el None
                        [ center ]
                        (column None
                            [ EA.width (EA.px 360), spacing 5 ]
                            [ row None
                                [ spacing 10, EA.padding 10 ]
                                [ column Deperture
                                    [ EA.width (EA.percent 50), spacing 10, EA.padding 10, EE.onClick ClickDeperture ]
                                    [ el BoxTitle [] <| Element.text "出発地"
                                    , el BoxMain [] <| todofuken model.depPrefNum
                                    ]
                                , column Destination
                                    [ EA.width (EA.percent 50), spacing 10, EA.padding 10, EE.onClick ClickDestination ]
                                    [ el BoxTitle [] <| Element.text "目的地"
                                    , el BoxMain [] <| todofuken model.destPrefNum
                                    ]
                                ]
                            , row None
                                [ spacing 10, EA.padding 10, spread ]
                                [ column Date
                                    [ EA.width (EA.percent 50), spacing 10, EA.padding 10, EE.onClick (ToggleDatePicker CheckIn) ]
                                    [ el BoxTitle [] <| Element.text "出発日"
                                    , el BoxMain [] <| Element.text (DateFormat.format config "%b/%-d (%a)" model.dateCheckIn)
                                    ]
                                , el Submit
                                    [ if model.toggleResult then
                                        EE.onClick CloseResult
                                      else
                                        EE.onClick SubmitSearch
                                    , alignBottom
                                    , EA.padding 10
                                    , EA.height (EA.px 42)
                                    , EA.width (EA.px 42)
                                    ]
                                  <|
                                    Element.html
                                        (i [ class "material-icons md-24 pointer" ]
                                            (if model.toggleResult then
                                                [ text "expand_more" ]
                                             else
                                                [ text "search" ]
                                            )
                                        )
                                ]
                            ]
                        )
                    )
                , el None
                    [ EA.width (EA.percent 100), EA.class "overflow-y-scroll" ]
                    (el None [ center, EA.width (EA.px 360) ] <| Element.html <| searchResultBusList model)
                ]
        , if model.datePickerShow then
            Search.DatePicker.view model.datePickerModel
          else
            text ""
        ]


todofuken : Int -> Element.Element MyStyle a msg
todofuken code =
    Element.text (Todofuken.fromCode code |> Maybe.map .name |> Maybe.withDefault " ")


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
                    div [ class "pa3 ma3 hover-bg-black-10 shadow-1" ]
                        [ div [ class "f3" ] [ text bus.companyName ]
                        , div [] [ text bus.name ]
                        , div [] [ text <| "￥" ++ toString bus.amount, text <| " 空席 :" ++ bus.vacancy ]
                        , div [] (List.map showCityAndTime (organizeCities bus.depTime))
                        , div [ class "dib" ] [ i [ class "material-icons" ] [ text "navigate_next" ] ]
                        , div [ class "dib" ] (List.map showCityAndTime (organizeCities bus.destTime))
                        ]
            in
            div [ class "mt3" ] (List.map list buses)

        RemoteData.Failure error ->
            div [] []


organizeCities : String -> List ( String, String )
organizeCities str =
    str
        |> String.dropLeft 2
        |> String.dropRight 2
        |> String.split "], ["
        |> List.map
            (\s ->
                ( Maybe.withDefault "" <| List.head <| String.split "," s
                , Maybe.withDefault "" <| List.head <| Maybe.withDefault [] <| List.tail <| String.split "," s
                )
            )


showCityAndTime : ( String, String ) -> Html msg
showCityAndTime ( city, time ) =
    div [ class "dib" ] [ text city, br [] [], text time ]


searchFormView : Model -> Html Msg
searchFormView model =
    div [ class "absolute absolute--fill bg-black-50 fixed z2" ]
        [ div [ class "absolute absolute--fill", onClick (ToggleSearch model.citySearch) ] []
        , div
            [ class "w-80 center br2 bg-near-white navy absolute absolute--fill ma-auto bg-white-10 shadow-2"
            , style [ ( "height", "90%" ) ]
            ]
            [ div [ class "h-100 pa3 br2 flex flex-column flex-nowrap" ]
                [ div [ class "f4 mb2" ]
                    (if model.citySearch == Model.Deperture then
                        [ text "出発地" ]
                     else
                        [ text "目的地" ]
                    )
                , input
                    [ id "search-place"
                    , type_ "search"
                    , class "f6 f5-l input-reset bn pa3 br2 w-100 shadow-2"
                    , placeholder "場所を入力"
                    , onInput StartSearching
                    ]
                    []
                , div [ class "flex-auto overflow-auto relative" ]
                    [ ul [ class "list pa1 bt bw1 bb b--naby w-100 absolute top-0 left-0" ]
                        (List.map (searchResultList model.citySearch) model.citySearchResult)
                    ]
                ]
            ]
        ]


searchResultList : Model.DepOrDest -> Model.City -> Html Msg
searchResultList depDest city =
    li
        [ class "b--navy bb bw1 br2 br--top ma1 ph2 pv2 f4 hover-bg-black-20 pointer"
        , onClick (SelectCity city depDest)
        ]
        [ text city.city ]


howManyPeopleView : Model -> Html Msg
howManyPeopleView model =
    div [ class "absolute absolute--fill bg-black-50 fixed z2" ]
        [ div [ class "absolute absolute--fill", onClick ToggleNumOfPeople ] []
        , div
            [ class "w-60 center br2 bg-near-white navy absolute absolute--fill ma-auto bg-white-10 shadow-2"
            , style [ ( "height", "90%" ) ]
            ]
            [ div
                [ class "h-100 pa3 br2 flex flex-column flex-nowrap" ]
                [ div [ class "f3 mb2" ] [ text "人数" ]
                , div [ class "dib w-100 ba br2 tc mb2 pa2 b--navy" ]
                    [ span [ class "f5 pb1" ] [ text "大人: " ]
                    , input
                        [ class "f6 f5-l input-reset bn w2 pa1 br2"
                        , onInput SetNumOfAdult
                        , type_ "number"
                        , value (toString model.numOfPeople.adult)
                        ]
                        []
                    , span [ class "f5 pb1" ] [ text " , 子供: " ]
                    , input
                        [ class "f6 f5-l input-reset bn w2 pa1 br2"
                        , onInput SetNumOfChild
                        , type_ "number"
                        , value (toString model.numOfPeople.child)
                        ]
                        []
                    ]
                , div [ class "ws-normal tc flex-auto overflow-auto relative" ]
                    [ div [ class "w-100 absolute top-0 left-0" ]
                        [ div
                            [ class numOfPeopleButtonClass
                            , onClick (SelectNumOfPeople 1 0)
                            , classList
                                [ ( "bg-white", (model.numOfPeople.adult == 1) && (model.numOfPeople.child == 0) )
                                ]
                            ]
                            [ span [ class "h-75 db" ]
                                [ i [ class "material-icons md-66 mt3 mb1 dib v-btm" ] [ text "person" ]
                                ]
                            , span [ class "f6 pb1" ] [ text "大人: 1" ]
                            ]
                        , div
                            [ class numOfPeopleButtonClass
                            , onClick (SelectNumOfPeople 0 1)
                            , classList
                                [ ( "bg-white", (model.numOfPeople.adult == 0) && (model.numOfPeople.child == 1) )
                                ]
                            ]
                            [ span [ class "h-75 db" ]
                                [ i [ class "material-icons md-40 mt4 mb2 dib v-btm" ] [ text "person" ]
                                ]
                            , span [ class "f6 pb1" ] [ text "子供: 1" ]
                            ]
                        , div
                            [ class numOfPeopleButtonClass
                            , onClick (SelectNumOfPeople 2 0)
                            , classList
                                [ ( "bg-white", (model.numOfPeople.adult == 2) && (model.numOfPeople.child == 0) )
                                ]
                            ]
                            [ span [ class "h-75 db" ]
                                [ i [ class "material-icons md-66 mt3 mb1 dib v-btm" ] [ text "people" ]
                                ]
                            , span [ class "f6 pb1" ] [ text "大人: 2" ]
                            ]
                        , div
                            [ class numOfPeopleButtonClass
                            , onClick (SelectNumOfPeople 2 1)
                            , classList
                                [ ( "bg-white", (model.numOfPeople.adult == 2) && (model.numOfPeople.child == 1) )
                                ]
                            ]
                            [ span [ class "h-75 db" ]
                                [ i [ class "material-icons md-72 mt3 mb2 dib v-btm" ] [ text "people" ]
                                , i [ class "material-icons md-40 mt3 mb3 dib v-btm" ] [ text "person" ]
                                ]
                            , span [ class "f6 pb1" ] [ text "大人: 2, 子供: 1" ]
                            ]
                        , div
                            [ class numOfPeopleButtonClass
                            , onClick (SelectNumOfPeople 2 2)
                            , classList
                                [ ( "bg-white", (model.numOfPeople.adult == 2) && (model.numOfPeople.child == 2) )
                                ]
                            ]
                            [ span [ class "h-75 db" ]
                                [ i [ class "material-icons md-72 mt3 mb2 dib v-btm" ] [ text "people" ]
                                , i [ class "material-icons md-40 mt3 mb3 dib v-btm" ] [ text "people" ]
                                ]
                            , span [ class "f6 pb1" ] [ text "大人: 2, 子供: 2" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


numOfPeopleButtonClass : String
numOfPeopleButtonClass =
    "dib w4 h4 ba br2 b--white tc ma1 bg-black-10 pointer v-top shadow-1"
