module Search.View exposing (howManyPeopleView, searchFormView, searchFromMapView)

import Color exposing (rgb)
import Element exposing (column, el, row)
import Element.Attributes as EA exposing (center, spacing)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Search.DatePicker
import Search.Map as Map
import Style
import Style.Border as Border
import Style.Color as Color
import Todofuken


type MyStyle
    = None
    | Header
    | Deperture
    | Destination


styleSheet : Style.StyleSheet MyStyle a
styleSheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style Header [ Border.bottom 1 ]
        , Style.style Deperture [ Border.bottom 2, Color.border (rgb 48 63 159) ]
        , Style.style Destination [ Border.bottom 2, Color.border (rgb 255 87 34) ]
        ]


searchFromMapView : Model -> Html Msg
searchFromMapView model =
    div [ class "absolute absolute--fill bg-white fixed z2" ]
        [ Element.layout styleSheet <|
            column None
                [ EA.height (EA.percent 100) ]
                [ el Header [] <| Element.html (i [ class "material-icons md-48 pointer", onClick ToggleMap ] [ text "navigate_before" ])
                , el None
                    [ center, EA.width (EA.px 360) ]
                    (column None
                        [ spacing 10 ]
                        [ el None [] (Element.html (Map.maps model))
                        , row None
                            [ spacing 10, EA.padding 10 ]
                            [ column None
                                [ EA.width (EA.percent 50), spacing 10 ]
                                [ Element.text "出発地"
                                , el Deperture [] (todofuken model.depPrefNum)
                                ]
                            , column None
                                [ EA.width (EA.percent 50), spacing 10 ]
                                [ Element.text "目的地"
                                , el Destination [] (todofuken model.destPrefNum)
                                ]
                            ]
                        ]
                    )
                ]

        -- Element.html <|
        --     div
        --         [ class "h-100 flex flex-column" ]
        --         [ div [ class "db pa2 bb" ] [ i [ class "material-icons md-48 pointer", onClick ToggleMap ] [ text "navigate_before" ] ]
        --         , div
        --             [ class "flex-auto overflow-auto center"
        --             , style
        --                 [ ( "box-shadow", "inset 0px 0px 10px 5px rgba(0,0,0,0.4)" )
        --                 , ( "width", "340px" )
        --                 ]
        --             ]
        --             [ Map.maps model ]
        --         , div [ class "h5 w5 center flex f5" ]
        --             [ div [ class "dib ma2 pa2 h4 w-50 align-top" ]
        --                 [ text "出発地"
        --                 , div [ class "mv2 bb b--dark-blue" ] [ todofuken model.depPrefNum ]
        --                 ]
        --             , div [ class "dib ma2 pa2 h4 w-50 align-top" ]
        --                 [ text "目的地"
        --                 , div [ class "mt2 bb b--orange" ] [ todofuken model.destPrefNum ]
        --                 ]
        --             ]
        --         ]
        ]


todofuken : Int -> Element.Element MyStyle a msg
todofuken code =
    Element.text (Todofuken.fromCode code |> Maybe.map .name |> Maybe.withDefault " ")


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
