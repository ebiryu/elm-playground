module Search.View exposing (howManyPeopleView, searchFormView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model, Route(..))
import Msg exposing (Msg(..))
import Search.DatePicker


view : Model -> Html Msg
view model =
    div [ class "absolute absolute--fill bg-black-50 fixed z2" ]
        [ div [ class "absolute absolute--fill", onClick (ToggleSearch model.citySearch) ] []
        , div
            [ class "w-80 center br2 bg-near-white navy absolute absolute--fill ma-auto bg-white-10 shadow-2"
            , style [ ( "height", "90%" ) ]
            ]
            [ searchFormView model
            , howManyPeopleView model
            ]
        , if model.datePickerShow then
            Search.DatePicker.view model.datePickerModel
          else
            text ""
        ]


inlineClass : String
inlineClass =
    "h-100 pa3 br2"


searchFormView : Model -> Html Msg
searchFormView model =
    div [ class "absolute absolute--fill bg-black-50 fixed z2" ]
        [ div [ class "absolute absolute--fill", onClick (ToggleSearch model.citySearch) ] []
        , div
            [ class "w-80 center br2 bg-near-white navy absolute absolute--fill ma-auto bg-white-10 shadow-2"
            , style [ ( "height", "90%" ) ]
            ]
            [ div
                [ class "h-100 pa3 br2" ]
                [ div [ class "f3 mb2" ]
                    (if model.citySearch == Model.Deperture then
                        [ text "出発地" ]
                     else
                        [ text "目的地" ]
                    )
                , div [ class "h-75" ]
                    [ input
                        [ id "search-place"
                        , type_ "search"
                        , class "f6 f5-l input-reset bn pa3 br2 w-100 shadow-2"
                        , placeholder "場所を入力"
                        , onInput StartSearching
                        ]
                        []
                    , ul [ class "list pa1 overflow-auto h-100 bt bw1 bb b--naby" ]
                        (List.map (searchResultList model.citySearch) model.citySearchResult)
                    ]
                ]
            ]
        ]


searchResultList : Model.DepOrDest -> Model.City -> Html Msg
searchResultList depDest city =
    li
        [ class "b--navy bb bw1 br2 br--top ma1 ph2 pv3 f4 hover-bg-black-20 pointer"
        , onClick (SelectCity city depDest)
        ]
        [ text city.city ]


howManyPeopleView : Model -> Html Msg
howManyPeopleView model =
    div [ class "absolute absolute--fill bg-black-50 fixed z2" ]
        [ div [ class "absolute absolute--fill", onClick ToggleNumOfPeople ] []
        , div
            [ class "w-80 center br2 bg-near-white navy absolute absolute--fill ma-auto bg-white-10 shadow-2"
            , style [ ( "height", "90%" ) ]
            ]
            [ div
                [ class "h-100 pa3 br2" ]
                [ div [ class "f3 mb2" ] [ text "人数" ]
                , div [ class "w-90-m w-70-l center ws-normal flex-auto overflow-auto tc" ]
                    [ div [ class "dib w-90 ba br2 tc mb2 pa2 b--navy" ]
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
                    , div
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


numOfPeopleButtonClass : String
numOfPeopleButtonClass =
    "dib w4 h4 ba br2 b--white tc ma1 bg-black-10 pointer v-top shadow-1"
