module Search.Map exposing (..)

import Animation
import Draggable
import Html
import Html.Attributes as HtmlA
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg)
import MultiTouch
import Svg exposing (..)
import Svg.Attributes exposing (d, fill, height, points, scale, stroke, style, transform, viewBox, width, x, y)
import Svg.Events exposing (on, onClick, onMouseOut, onMouseOver)
import Touch
import VirtualDom


maps : Model -> Html.Html Msg
maps model =
    let
        ( cx, cy ) =
            ( model.mapPosition.x, model.mapPosition.y )

        ( halfWidth, halfHeight ) =
            ( 600 / model.mapZoom / 2, 500 / model.mapZoom / 2 )

        panning =
            "translate(" ++ toString (halfWidth + cx) ++ ", " ++ toString (halfHeight + cy) ++ ")"

        zooming =
            "translate(" ++ toString (-halfWidth - cx) ++ ", " ++ toString (-halfHeight - cy) ++ ")" ++ "scale(" ++ toString model.mapZoom ++ ")" ++ "translate(" ++ toString (halfWidth + cx) ++ ", " ++ toString (halfHeight + cy) ++ ")"

        offset =
            "translate(" ++ toString (-halfWidth * model.mapZoom) ++ ", " ++ toString (-halfHeight * model.mapZoom) ++ ")"
    in
    Html.div [ HtmlA.class "h-100", MultiTouch.onStart (Msg.MultiStart 50), MultiTouch.onMove Msg.MultiMove ]
        [ svg (List.concat [ Animation.render model.animStyleOfMapDiv, [ height "100%", width "100%", viewBox "0 0 600 500", Draggable.mouseTrigger "" Msg.DragMsg, onScroll Msg.MapZoom ] ])
            [ g [ transform (panning ++ " " ++ zooming) ]
                [ g [ transform offset ]
                    [ g [] (mapClick model)
                    , polygon [ borderStyle, fill "#fff", points "67.771,497.093 66.352,497.093 66.352,434.494 13.083,434.494 13.083,433.074 67.771,433.074 " ] []
                    ]
                ]
            ]
        ]


svgSize : Model -> List (Svg.Attribute msg)
svgSize { device } =
    let
        h =
            toString (device.height - 258) ++ "px"
    in
    [ width "100%", height "100%" ]


mapClick : Model -> List (Svg Msg)
mapClick model =
    List.map
        (\( i, pref ) ->
            Svg.map
                (\e ->
                    case e of
                        Click ->
                            Msg.ClickPrefecture i

                        Hover ->
                            Msg.HoverPrefecture i

                        HoverOut ->
                            Msg.HoverOutMap

                        SingleTouchStart event ->
                            Msg.MultiStart i event

                        SingleLeave event ->
                            Msg.SingleEnd i event
                )
                pref
        )
        (indexedPrefs model)


indexedPrefs : Model -> List ( Int, Svg Event )
indexedPrefs model =
    List.indexedMap
        (\i ( f, a ) ->
            if i == (model.depPrefNum - 1) then
                (,) i <| f (List.concat [ depPrefAttribute, a ]) []
            else if i == (model.destPrefNum - 1) then
                (,) i <| f (List.concat [ destPrefAttribute, a ]) []
            else
                (,) i <| f (List.concat [ prefAttribute, a ]) []
        )
        prefElements


prefAttribute : List (Svg.Attribute Event)
prefAttribute =
    [ MultiTouch.onStart SingleTouchStart, MultiTouch.onEnd SingleLeave, onClick Click, onMouseOver Hover, onMouseOut HoverOut, prefStyle ]


depPrefAttribute : List (Svg.Attribute Event)
depPrefAttribute =
    [ MultiTouch.onStart SingleTouchStart, onClick Click, onMouseOver Hover, onMouseOut HoverOut, depPrefStyle ]


destPrefAttribute : List (Svg.Attribute Event)
destPrefAttribute =
    [ MultiTouch.onStart SingleTouchStart, onClick Click, onMouseOver Hover, onMouseOut HoverOut, destPrefStyle ]


type Event
    = Click
    | Hover
    | HoverOut
    | SingleTouchStart Touch.Event
    | SingleLeave Touch.Event


prefStyle : Svg.Attribute msg
prefStyle =
    Svg.Attributes.style "fill:#aaa; cursor:pointer"


depPrefStyle : Svg.Attribute msg
depPrefStyle =
    Svg.Attributes.style "fill:#303F9F; cursor:pointer"


destPrefStyle : Svg.Attribute msg
destPrefStyle =
    Svg.Attributes.style "fill:#FF5722; cursor:pointer"


borderStyle : Svg.Attribute msg
borderStyle =
    Svg.Attributes.style "fill:#aaa;"


onScroll : (Float -> msg) -> Svg.Attribute msg
onScroll zoom =
    let
        options =
            VirtualDom.Options True True
    in
    VirtualDom.onWithOptions "wheel" options (Decode.map zoom <| Decode.field "deltaY" Decode.float)


prefElements : List ( List (Svg.Attribute msg) -> List (Svg.Svg msg) -> Svg.Svg msg, List (Svg.Attribute msg) )
prefElements =
    [ ( path, [ stroke "#fff", d "M558.193,113.86c4.429,0,8.052-3.624,8.052-8.053V27.819c0-4.429-3.623-8.053-8.052-8.053H425.551 c-4.43,0-8.052,3.624-8.052,8.053v99.398c0,4.429,3.622,8.053,8.052,8.053h21.082c4.429,0,8.052-3.624,8.052-8.053v-5.305 c0-4.429,3.623-8.052,8.052-8.052H558.193z" ] )
    , ( path, [ stroke "#fff", d "M472.867,177.426h1.433h57.013v-20.581c0-4.429-3.623-8.052-8.052-8.052h-99.4 c-4.429,0-8.053,3.624-8.053,8.052v20.581H472.867z" ] )
    , ( rect, [ x "474.3", y "178.855", stroke "#fff", width "57.013", height "39.514" ] )
    , ( rect, [ x "474.3", y "219.799", stroke "#fff", width "57.013", height "44.696" ] )
    , ( rect, [ x "415.808", y "178.855", stroke "#fffff", width "57.06", height "39.514" ] )
    , ( polygon, [ stroke "#fff", points "439.373,264.495 472.867,264.495 472.867,219.799 415.808,219.799 415.808,250.699 439.373,250.699" ] )
    , ( rect, [ x "439.373", y "265.928", stroke "#fff", width "91.939", height "39.191" ] )
    , ( polygon, [ stroke "#fff", points "497.111,364.525 531.312,364.525 531.229,306.363 497.027,306.363" ] )
    , ( rect, [ x "457.205", y "306.363", stroke "#fff", width "38.461", height "43.135" ] )
    , ( rect, [ x "415.531", y "306.363", stroke "#fff", width "40.223", height "43.135" ] )
    , ( rect, [ x "415.531", y "350.922", stroke "#fff", width "80.135", height "38.486" ] )
    , ( path, [ stroke "#fff", d "M497.111,365.955v55.52h-0.168v35.881c0,4.448,3.623,8.086,8.052,8.086h18.266 c4.429,0,8.052-3.638,8.052-8.086v-91.4H497.111z" ] )
    , ( path, [ stroke "#fff", d "M495.666,390.828h-56.292v31.427h37.85v-0.78c0-4.447,3.623-8.086,8.052-8.086h3.616 c2.838,0,5.34,1.498,6.774,3.742V390.828z" ] )
    , ( path, [ stroke "#fff", d "M439.374,423.808v32.966c1.382-2.25,4.016-3.778,7.024-3.778h22.773c4.429,0,8.052-3.639,8.052-8.088 v-21.1H439.374z" ] )
    , ( path, [ stroke "#fff", d "M390.36,304.943h47.39v-52.942h-21.942v4.807c0,4.429-3.622,8.056-8.052,8.059l-17.396,0.014V304.943z" ] )
    , ( path, [ stroke "#fff", d "M351.459,304.943h37.422v-40.062l-31.048,0.024c-2.591,0.002-4.898-1.241-6.374-3.155V304.943z" ] )
    , ( path, [ stroke "#fff", d "M350.039,304.943v-46.104c-0.162-0.635-0.258-1.295-0.258-1.979v-8.776c0-4.43-3.623-8.053-8.053-8.053 h-10.152c-4.429,0-8.053,3.623-8.053,8.053v56.859H350.039z" ] )
    , ( path, [ stroke "#fff", d "M323.523,306.363v11.841c0,4.43-3.624,8.054-8.052,8.054H301.5v23.24h47.173v-43.135H323.523z" ] )
    , ( rect, [ x "403.441", y "390.828", stroke "#fff", width "34.309", height "31.427" ] )
    , ( polygon, [ stroke "#fff", points "401.979,389.408 414.055,389.408 414.055,306.363 380.809,306.363 380.809,422.255 401.979,422.255 " ] )
    , ( rect, [ x "350.094", y "306.157", stroke "#fff", width "28.906", height "116.098" ] )
    , ( path, [ stroke "#fff", d "M390.881,423.753v43.888l38.896,0.055c4.43,0,8.052-3.296,8.052-7.324c0-1.291,0-36.672,0-36.672 L390.881,423.753z" ] )
    , ( path, [ stroke "#fff", d "M389.422,423.808h-8.613v-0.108h-30.715v48.993c1.21-2.925,4.095-4.997,7.442-4.997h31.886V423.808z" ] )
    , ( path, [ stroke "#fff", d "M348.673,389.8l-24.155-0.008v100.997l16.914,0.008c3.179,0,5.934-1.87,7.241-4.561V389.8z" ] )
    , ( rect, [ x "324.704", y "350.922", stroke "#fff", width "23.969", height "37.329" ] )
    , ( polygon, [ stroke "#fff", points "299.956,326.258 275.285,326.258 275.285,326.271 275.285,327.931 275.285,388.315 323.078,388.315 323.079,350.729 299.956,350.729 " ] )
    , ( path, [ stroke "#fff", d "M298.737,445.374V389.8h-23.452v28.866c0.144,0.601,0.229,1.224,0.229,1.866v24.842H298.737z" ] )
    , ( path, [ stroke "#fff", d "M273.838,326.258H246.25v86.222h21.211c2.592,0,4.901,1.246,6.376,3.164v-87.713v-1.66V326.258z" ] )
    , ( polygon, [ stroke "#fff", points "300.144,445.374 300.144,446.812 300.144,465.706 323.079,465.706 323.079,389.8 300.144,389.8" ] )
    , ( path, [ stroke "#fff", d "M298.737,446.812h-23.224v35.934c0,4.429,3.624,8.052,8.053,8.052h39.513v-23.685h-24.342V446.812z" ] )
    , ( rect, [ x "215.158", y "326.258", stroke "#fff", width "29.342", height "39.3" ] )
    , ( rect, [ x "185.097", y "326.258", stroke "#fff", width "28.628", height "39.3" ] )
    , ( rect, [ x "215.158", y "366.997", stroke "#fff", width "29.342", height "45.482" ] )
    , ( rect, [ x "185.097", y "366.997", stroke "#fff", width "28.628", height "45.482" ] )
    , ( path, [ stroke "#fff", d "M183.665,326.258h-18.982c-4.429,0-8.052,3.624-8.052,8.052v70.117c0,4.429,3.624,8.053,8.052,8.053 h18.982V326.258z" ] )
    , ( path, [ stroke "#fff", d "M209.456,458.21v30.822h45.288c4.429,0,8.053-3.623,8.053-8.053v-22.77H209.456z" ] )
    , ( path, [ stroke "#fff", d "M209.456,456.778h53.341v-21.941c0-4.429-3.624-8.053-8.053-8.053h-45.288V456.778z" ] )
    , ( path, [ stroke "#fff", d "M208.023,456.778v-29.994h-44.011c-4.429,0-8.052,3.624-8.052,8.053v21.941H208.023z" ] )
    , ( path, [ stroke "#fff", d "M208.023,458.21H155.96v22.77c0,4.43,3.624,8.053,8.052,8.053h44.011V458.21z" ] )
    , ( path, [ stroke "#fff", d "M76.257,365.567h64.25v-32.27c0-4.43-3.624-8.053-8.053-8.053H76.257V365.567z" ] )
    , ( path, [ stroke "#fff", d "M74.825,366.987v-1.42v-40.322H57.067v50.77h12.257c2.125,0,4.058,0.84,5.5,2.197V366.987z" ] )
    , ( path, [ stroke "#fff", d "M55.663,325.245H44.372c-4.429,0-8.052,3.623-8.052,8.053v34.664c0,4.429,3.624,8.053,8.052,8.053h11.291 V325.245z" ] )
    , ( path, [ stroke "#fff", d "M104.357,444.871v-77.884h-28.1v13.018c0.705,1.195,1.116,2.581,1.115,4.062l-0.029,60.804H104.357z" ] )
    , ( rect, [ x "105.81", y "366.987", stroke "#fff", width "34.698", height "39.006" ] )
    , ( rect, [ x "105.81", y "407.413", stroke "#fff", width "34.698", height "37.458" ] )
    , ( path, [ stroke "#fff", d "M77.343,446.277l-0.017,34.702c-0.002,4.43,3.62,8.053,8.049,8.053h47.08c4.429,0,8.053-3.623,8.053-8.053 v-34.702H77.343z" ] )
    , ( path, [ stroke "#fff", d "M62.363,480.969c0,4.43-3.624,8.055-8.053,8.055h-8.284c-4.429,0-8.053-3.625-8.053-8.055v-27.795 c0-4.428,3.624-8.053,8.053-8.053h8.284c4.429,0,8.053,3.625,8.053,8.053V480.969z" ] )
    ]
