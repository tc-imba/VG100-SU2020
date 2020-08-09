module GameView exposing (..)

import Ball exposing (..)
import Block exposing (..)
import Bob2 exposing (bob2)
import Debug
import Geometry exposing (..)
import Html exposing (..)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Paddle exposing (..)
import Parameters exposing (..)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)


gameColors =
    { bg = "#1a1a2b"
    , blockColor1 = "#524447"
    , blockColor2 = "#2f2c2d"
    , blockColor3 = "#3d3c3c"
    , explodeblockColor = "#af3b50"
    , expblockColor = "#e25f08"
    , grassColor = "#0fe208"
    , dirtColor = "#66320e"
    , blockStrokeColor = "#6d668d"
    , panelColor = "#010402"
    , radarMonitorColor = "#010402"
    , radarOuterColor = "#101010"
    , padColor = "#F0793E"
    , ballColor = "yellow"
    , waterColor = "#558DFF"
    , captionColor = "#7a88a4"
    , textbg = "#101010"
    }


blockColor : Int -> String
blockColor level =
    if level == 1 then
        gameColors.blockColor1

    else if level == 2 then
        gameColors.blockColor2

    else
        gameColors.blockColor3


renderGame : Model -> Html Msg
renderGame model =
    let
        yStep =
            gameParams.yStep

        coverOpacity =
            if model.currentLevel <= 3 && model.pad.y > negate 16 * yStep then
                0

            else if model.currentLevel <= 3 && model.pad.y <= negate 16 * yStep then
                Basics.min ((negate 16 * yStep - model.pad.y) / (8 * yStep)) 1

            else
                0
    in
    div
        [ HA.style "position" "absolute"
        , HA.style "width" "100%"
        , HA.style "padding" "0px"
        , HA.style "text-align" "center"
        , HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "background-color" gameColors.panelColor
        ]
        [ svg
            [ SA.viewBox
                ("0 0 "
                    ++ String.fromInt gameParams.w
                    ++ " "
                    ++ String.fromInt gameParams.h
                )
            , SA.width (model.screenSize |> Tuple.first |> String.fromFloat)
            , SA.height (model.screenSize |> Tuple.second |> String.fromFloat)
            , SA.fill "black"
            ]
            [
              rect
                [ SA.x "0"
                , SA.y "0"
                , gameParams.w |> String.fromInt |> SA.width
                , gameParams.h |> String.fromInt |> SA.height
                , gameColors.bg |> SA.fill
                ]
                []
            , rect
                [ gameParams.w - gameParams.userPanelWidth |> String.fromInt |> SA.x
                , "0" |> SA.y
                , gameParams.userPanelWidth |> String.fromInt |> SA.width
                , gameParams.h |> String.fromInt |> SA.height
                , gameColors.panelColor |> SA.fill
                ]
                []
            , renderRadar ( model.pad.x, model.pad.y ) model.targetPos
            , rect
                [ SA.x "717"
                , SA.y "240"
                , SA.width "264"
                , SA.height "210"
                , SA.fill gameColors.textbg
                , SA.fillOpacity "0.8"
                , SA.rx "10"
                ]
                []
            , caption 800 270 "Move Pad:"
            , caption 800 300 "a←"
            , caption 860 300 "d→"
            , caption 795 330 ("Block Hp: " ++ ( Debug.toString (2 ^ ( model.currentLevel - 1 ))))
            , caption 780 360 ("Attack of ball: " ++ (Debug.toString model.properties.atk))
            , captiongd 820 390 "Coins:"
            , captiongd 830 420 (Debug.toString model.asset)
            , button [ onClick ClickHome ] [ Html.text "Back to Home!" ]
            , svg
                [ SA.viewBox
                    ("0 0 "
                        ++ ((gameParams.w - gameParams.userPanelWidth) |> String.fromInt)
                        ++ " "
                        ++ String.fromInt gameParams.h
                    )
                , SA.width ((gameParams.w - gameParams.userPanelWidth) |> String.fromInt)
                , SA.height (String.fromInt gameParams.h)
                , SA.fill "black"
                ]
                ([ renderWater model.gamerPos model.waterLevel ]
                    ++ List.map (\x -> renderBlock model.gamerPos model.currentLevel x) model.block
                    ++ [ renderPad model.gamerPos model.pad ]
                    ++ [ renderBall model.gamerPos model.ball ]
                    ++ [ darkFilter model ]
                )
            , rect
                [ SA.x "0"
                , SA.y "0"
                , gameParams.h |> String.fromFloat |> SA.height
                , gameParams.w + 90 |> String.fromFloat |> SA.width
                , SA.fill "black"
                , coverOpacity |> String.fromFloat |> SA.fillOpacity
                ]
                []
            , text_
                [ 40 |> String.fromInt |> SA.x
                , 350 |> String.fromInt |> SA.y
                , SA.fill "white"
                , SA.fontFamily "sans-serif"
                , SA.fontSize "120"
                ]
                [ (if model.currentPage == LevelFinish then
                    "Level Clear!"

                   else if model.state == Stopped then
                    "Nice~ Try~"

                   else
                    ""
                  )
                    |> Svg.text
                ]
            , text_
                [ 450 |> String.fromInt |> SA.x
                , 500 |> String.fromInt |> SA.y
                , SA.fill "white"
                , SA.fontFamily "sans-serif"
                , SA.fontSize "40"
                ]
                [ (if model.currentPage == LevelFinish then
                    "Press Enter to Continue"

                   else if model.state == Stopped then
                    "Press Enter back to levels page"

                   else
                    ""
                  )
                    |> Svg.text
                ]
            , listenMusic model
            ]
        ]

listenMusic model =
    if model.state == Model.Playing then
        audio
            [ HA.src "https://link.hhtjim.com/163/547226518.mp3"
            , HA.autoplay True
            , HA.loop True
            ]
            []

    else
        audio
            [ HA.src "https://link.hhtjim.com/163/527496589.mp3"
            , HA.autoplay True
            , HA.loop True
            ]
            []


darkFilter : Model -> Svg msg
darkFilter model =
    let
        x0 =
            Tuple.first model.gamerPos

        y0 =
            Tuple.second model.gamerPos

        cx =
            model.ball.x - x0

        cy =
            model.ball.y - y0

    in
    svg []
        [ Svg.defs []
            [ Svg.radialGradient
                [ SA.id "RGfilter"
                , 0.5 |> String.fromFloat |> SA.cx
                , 0.5 |> String.fromFloat |> SA.cy
                , 0.12 |> String.fromFloat |> SA.r
                ]
                [ Svg.stop [ offset "20%", stopColor "orange" ] []
                , Svg.stop [ offset "100%", stopColor "black" ] []
                ]
            ]
        , circle
            [ cx |> String.fromFloat |> SA.cx
            , cy |> String.fromFloat |> SA.cy
            , 2000 |> String.fromFloat |> SA.r
            , SA.fill "url(#RGfilter)"
            , SA.fillOpacity "0.5"
            ]
            []
        ]


caption : Int -> Int -> String -> Svg msg
caption x y s =
    text_
        [ x |> String.fromInt |> SA.x
        , y |> String.fromInt |> SA.y
        , SA.fill gameColors.captionColor
        , SA.fontFamily "sans-serif"
        , SA.fontSize "20"
        ]
        [ s |> Svg.text ]


captiongd : Int -> Int -> String -> Svg msg
captiongd x y s =
    text_
        [ x |> String.fromInt |> SA.x
        , y |> String.fromInt |> SA.y
        , SA.fill gameColors.expblockColor
        , SA.fontFamily "sans-serif"
        , SA.fontSize "20"
        ]
        [ s |> Svg.text ]


renderBall : Point -> Ball -> Svg msg
renderBall pos ball =
    let
        x0 =
            Tuple.first pos

        y0 =
            Tuple.second pos
    in
    circle
        [ ball.x - x0 |> String.fromFloat |> SA.cx
        , ball.y - y0 |> String.fromFloat |> SA.cy
        , SA.r (String.fromFloat gameParams.ballRadius)
        , SA.fill gameColors.ballColor
        ]
        []


renderPad : Point -> Pad -> Html Msg
renderPad pos pad =
    let
        x0 =
            Tuple.first pos

        y0 =
            Tuple.second pos

        x =
            pad.x - x0

        y =
            pad.y - y0
    in
    svg []
        [ rect
            [ x + 1.0 |> String.fromFloat |> SA.x
            , y + 1.0 |> String.fromFloat |> SA.y
            , gameParams.padLength - 2.0 |> String.fromFloat |> SA.width
            , gameParams.padHeight + 2.0 |> String.fromFloat |> SA.height
            , SA.rx "1"
            , SA.fill gameColors.padColor
            ]
            []
        , svg [ SA.x "300", SA.y "149" ]
            [ bob2 ]
        ]


targetAngle : Point -> Point -> Float
targetAngle currentPos targetpos =
    let
        ytar =
            Tuple.second targetpos

        xtar =
            Tuple.first targetpos

        y =
            Tuple.second currentPos

        x =
            Tuple.first currentPos

        deltaY =
            y - ytar

        deltaX =
            xtar - x
    in
    atan2 deltaY deltaX


renderRadar : Point -> Point -> Html Msg
renderRadar currentPos targetPos =
    let
        theta =
            targetAngle currentPos targetPos

        distRatio =
            targetDistanceRatio currentPos targetPos
    in
    svg []
        ([ Svg.defs []
            [ Svg.radialGradient
                [ SA.id "RG1"
                , 0.5 + 0.3 * distRatio * cos theta |> String.fromFloat |> SA.cx
                , 0.5 - 0.3 * distRatio * sin theta |> String.fromFloat |> SA.cy
                , SA.r "0.1"
                ]
                [ Svg.stop [ offset "20%", stopColor "white" ] []
                , Svg.stop [ offset "100%", stopColor gameColors.radarMonitorColor ] []
                ]
            ]

         -- monitor in the radar
         , circle
            [ gameParams.radarX |> String.fromFloat |> SA.cx
            , gameParams.radarY |> String.fromFloat |> SA.cy
            , gameParams.radarR |> String.fromFloat |> SA.r
            , SA.fill "url(#RG1)"
            ]
            []

         -- central little circle
         , circle
            [ gameParams.radarX |> String.fromFloat |> SA.cx
            , gameParams.radarY |> String.fromFloat |> SA.cy
            , SA.r "6"
            , SA.fill "orange"
            ]
            []

         -- the outer ring
         , circle
            [ gameParams.radarX |> String.fromFloat |> SA.cx
            , gameParams.radarY |> String.fromFloat |> SA.cy
            , gameParams.radarR |> String.fromFloat |> SA.r
            , SA.strokeWidth "15"
            , SA.fill "none"
            , SA.stroke gameColors.radarOuterColor
            ]
            []
         ]
            ++ List.map lineInRadarH (List.map (\x -> toFloat x * pi / 20) (List.range -19 19))
            ++ List.map lineInRadarV (List.map (\x -> toFloat x * pi / 20) (List.range -19 19))
        )


lineInRadarH : Float -> Html Msg
lineInRadarH theta =
    line
        [ gameParams.radarX + (gameParams.radarR - gameParams.radarStrokeWidth / 2) * cos theta |> String.fromFloat |> SA.x1
        , gameParams.radarX - (gameParams.radarR - gameParams.radarStrokeWidth / 2) * cos theta |> String.fromFloat |> SA.x2
        , gameParams.radarY - (gameParams.radarR - gameParams.radarStrokeWidth / 2) * sin theta |> String.fromFloat |> SA.y1
        , gameParams.radarY - (gameParams.radarR - gameParams.radarStrokeWidth / 2) * sin theta |> String.fromFloat |> SA.y2
        , SA.stroke "white"
        , SA.fillOpacity "0.4"
        , SA.strokeWidth "0.25"
        ]
        []


lineInRadarV : Float -> Html Msg
lineInRadarV theta =
    line
        [ gameParams.radarX + (gameParams.radarR - gameParams.radarStrokeWidth / 2) * cos theta |> String.fromFloat |> SA.x1
        , gameParams.radarX + (gameParams.radarR - gameParams.radarStrokeWidth / 2) * cos theta |> String.fromFloat |> SA.x2
        , gameParams.radarY + (gameParams.radarR - gameParams.radarStrokeWidth / 2) * sin theta |> String.fromFloat |> SA.y1
        , gameParams.radarY - (gameParams.radarR - gameParams.radarStrokeWidth / 2) * sin theta |> String.fromFloat |> SA.y2
        , SA.stroke "white"
        , SA.fillOpacity "0.4"
        , SA.strokeWidth "0.25"
        ]
        []


targetDistanceRatio : Point -> Point -> Float
targetDistanceRatio currentPos targetPos =
    let
        dist =
            pointDiff currentPos targetPos |> norm

        initDistance =
            1250
    in
    dist / initDistance


renderBlock : Point -> Int -> Block -> Svg msg
renderBlock pos level block =
    let
        x0 =
            Tuple.first pos

        y0 =
            Tuple.second pos

        i =
            block.i

        j =
            block.j

        verticeX =
            gameParams.xStep * toFloat i

        verticeY =
            gameParams.yStep * toFloat j

        x =
            verticeX - x0

        y =
            verticeY - y0
    in
    svg []
        [ rect
            [ x |> String.fromFloat |> SA.x
            , y |> String.fromFloat |> SA.y
            , gameParams.xStep |> String.fromFloat |> SA.width
            , gameParams.yStep |> String.fromFloat |> SA.height
            , SA.fill
                (if List.any (\m -> m == ( i, j )) (boomlst level) then
                    gameColors.explodeblockColor

                 else if List.any (\m -> m == ( i, j )) (explst level) then
                    gameColors.expblockColor

                 else if level == 3 && j == -30 then
                    gameColors.grassColor

                 else if level == 3 && j > -30 && j < -16 then
                    gameColors.dirtColor

                 else
                    blockColor level
                )
            ]
            []
        , if gameParams.debugging == True then
            text_
                [ x + 0.3 * gameParams.xStep |> String.fromFloat |> SA.x
                , y + 0.3 * gameParams.yStep |> String.fromFloat |> SA.y
                , fill "white"
                , SA.fontSize "10"
                ]
                [ block.hp |> String.fromInt |> Svg.text ]

          else
            text_ [] []
        , if gameParams.debugging == True then
            text_
                [ x + 0.3 * gameParams.xStep |> String.fromFloat |> SA.x
                , y + 0.7 * gameParams.yStep |> String.fromFloat |> SA.y
                , fill "white"
                , SA.fontSize "10"
                ]
                [ block.i |> String.fromInt |> Svg.text
                , Svg.text ", "
                , block.j |> String.fromInt |> Svg.text
                ]

          else
            text_ [] []
        ]


renderWater : Point -> Float -> Svg msg
renderWater gamerPos waterLevel =
    let
        x0 =
            Tuple.first gamerPos

        y0 =
            Tuple.second gamerPos

        x =
            toFloat gameParams.iLeft * gameParams.xStep - x0

        y =
            toFloat gameParams.jBot * gameParams.yStep - waterLevel - y0

        waterWidth =
            toFloat (gameParams.iRight - gameParams.iLeft + 1) * gameParams.xStep

        waterHeight =
            waterLevel + 300.0
    in
    svg []
        [ rect
            [ x |> String.fromFloat |> SA.x
            , y |> String.fromFloat |> SA.y
            , SA.fill gameColors.waterColor
            , SA.fillOpacity "0.5"
            , waterWidth |> String.fromFloat |> SA.width
            , waterHeight |> String.fromFloat |> SA.height
            ]
            []
        ]

