module View exposing (..)

import Html exposing (Html, p)
import Html.Attributes exposing (attribute)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (Ball, Blocks, Blocktype(..), Landmine, LevelButton, Model, Monster, Msg(..), State(..))
import Model exposing (Msg)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp)
import Classic exposing (..)
import Time

renderBackground : List (Svg Msg)
renderBackground =
    [ image
        [ xlinkHref "https://i.loli.net/2020/06/18/Wtqp5vK2Qr8EjnD.jpg"
        , x "0"
        , y "0"
        , width "2000"
        , height "1000"
        ]
        []
    ]

renderBox : Model -> List (Svg Msg)
renderBox { coX, coY } =
    [ rect
         [ x (String.fromInt coX)
         , y (String.fromInt coY)
         , width "1498"
         , height "800"
         , stroke "orange"
         , fill "transparent"
         , strokeWidth "2"
         ]
         []
    ]

renderCover : Model -> List (Svg Msg)
renderCover model =
    if model.state == Cover then
        [ image
            [ xlinkHref "https://wx1.sbimg.cn/2020/06/19/co.jpg"
            , x "0"
            , y "0"
            , width "2000"
            , height "1000"
            , onClick PageOne
            ]
            []
        , text_
            [ x "580"
            , y "890"
            , fontSize "60"
            , fontStyle "normal"
            , fill "white"
            , fillOpacity "0.7"
            ]
            [ text "Click anywhere to continue!" ]
        ]
    else
        [rect [ x "0", y "0", width "1", height "1", fill "transparent"] []]


renderBox_ : Model -> List (Svg Msg)
renderBox_ { coX, coY, state } =
    if state == Stopped || state == SecondPage || state == ThirdPage || state == FourthPage || state == Level || state == Cover then
        [ image
            [ xlinkHref "https://wx1.sbimg.cn/2020/06/19/bb.jpg"
            , x (String.fromInt coX)
            , y (String.fromInt coY)
            , width "1498"
            , height "800"
            ]
            []
        ]
    else
        [rect [ x "0", y "0", width "1", height "1", fill "transparent"] []]


renderFilter : List (Svg Msg)
renderFilter =
   [defs []
       [ node "filter" [ attribute "filterUnits" "userSpaceOnUse", attribute "height" "900", id "MyFilter", attribute "width" "2000", attribute "x" "0", attribute "y" "0" ]
           [  node "feGaussianBlur" [ attribute "in" "SourceAlpha", attribute "result" "blur", attribute "stdDeviation" "4" ]
               []
           , node "feOffset" [ attribute "dx" "0.05", attribute "dy" "0.05", attribute "in" "blur", attribute "result" "offsetBlur" ]
               []
           , node "feSpecularLighting" [ attribute "in" "blur", attribute "lighting-color" "#bbbbbb", attribute "result" "specOut", attribute "specularConstant" ".75", attribute "specularExponent" "20", attribute "surfaceScale" "5" ]
               [ node "fePointLight" [ attribute "x" "-5000", attribute "y" "-10000", attribute "z" "20000" ]
                   []
               ]
           , node "feComposite" [ attribute "in" "specOut", attribute "in2" "SourceAlpha", attribute "operator" "in", attribute "result" "specOut" ]
               []
           , node "feComposite" [ attribute "in" "SourceGraphic", attribute "in2" "specOut", attribute "k1" "0", attribute "k2" "1", attribute "k3" "1", attribute "k4" "0", attribute "operator" "arithmetic", attribute "result" "litPaint" ]
               []
           , node "feMerge" []
               [ node "feMergeNode" [ attribute "in" "offsetBlur" ]
                   []
               , node "feMergeNode" [ attribute "in" "litPaint" ]
                   []
               ]
           ]
       ]
   ]


renderSpecial : Model -> List (Svg Msg)
renderSpecial { grid, bwid, bhei } =
    let
        mapSpecial : (Int, Int, Blocks) -> Svg Msg
        mapSpecial (n, m, block1) =
            case block1.types of
                Treasure ->
                    image [ xlinkHref "https://wx2.sbimg.cn/2020/06/19/treasure.png"
                          , x (String.fromInt n)
                          , y (String.fromInt (m - 5))
                          , width (String.fromFloat (bwid + 15))
                          , height (String.fromFloat (bhei + 10))
                          ]
                          []
                AddBall ->
                    image [ xlinkHref "https://wx1.sbimg.cn/2020/06/19/addball.png"
                          , x (String.fromInt n)
                          , y (String.fromInt m)
                          , width (String.fromFloat (bwid - 2))
                          , height (String.fromFloat (bhei - 2))
                          ]
                          []
                Crazy ->
                    image [ xlinkHref "https://wx2.sbimg.cn/2020/06/19/p.png"
                          , x (String.fromInt n)
                          , y (String.fromInt (m - 3))
                          , width (String.fromFloat (bwid + 10))
                          , height (String.fromFloat (bhei + 3))
                          ]
                          []
                _ ->
                    rect [] []
    in
    List.map (\ block -> mapSpecial ( block.bx, block.by, block )) grid

renderPreparingBrick : Model -> List (Svg Msg)
renderPreparingBrick model =
    let
        mapBrick : (Int, Int, Blocks) -> Svg Msg
        mapBrick (n, m, block1) =
            if model.waitBlocks /= Nothing then
                if List.member block1.types [ Treasure, AddBall, Crazy ] then
                    rect [] []
                else
                image
                    [ xlinkHref "https://i.loli.net/2020/06/19/sGzkAcmToqV9OPe.png"
                    , x (String.fromInt n)
                    , y (String.fromInt m)
                    , width (String.fromFloat (model.bwid - 2))
                    , height (String.fromFloat (model.bhei - 2))
                    ]
                    []
            else
                rect [] []
    in
    List.map (\ block -> mapBrick (block.bx, block.by, block) ) (Maybe.withDefault [] model.waitBlocks)


renderBrick : Model -> List (Svg Msg)
renderBrick { grid, bwid, bhei, level } =
    let
        mapBrick : (Int, Int, Blocks) -> Int -> Svg Msg
        mapBrick (n, m, block1) level1 =
            if List.member block1.types [ Treasure, AddBall, Crazy ] then
                rect [] []
            else
            image
                [ if (toFloat block1.hp) / (toFloat (Basics.min level1 15)) >= 3/4 then
                    xlinkHref "https://i.loli.net/2020/06/19/Z9NJK2wxXprulA8.png"
                  else if (toFloat block1.hp) / (toFloat (Basics.min level1 15)) >= 1/2 then
                    xlinkHref "https://i.loli.net/2020/06/19/2MikxNXcYC3qzE4.png"
                  else if (toFloat block1.hp) / (toFloat (Basics.min level1 15)) >= 1/4 then
                    xlinkHref "https://i.loli.net/2020/06/19/ufHx9cbWPv3UalC.png"
                  else
                    xlinkHref "https://i.loli.net/2020/06/19/GPrnteExLH5aOA6.png"
                , x (String.fromInt n)
                , y (String.fromInt m)
                , width (String.fromFloat (bwid - 2))
                , height (String.fromFloat (bhei - 2))
                ]
                []
    in
    List.map (\ block -> mapBrick (block.bx, block.by, block) level ) grid


renderBall : Model -> List (Svg Msg)
renderBall { ball, rball } =
    let
        mapBall : Ball -> Svg Msg
        mapBall ball1 =
            if ball1.speed <= 1/2.8 then
                circle
                    [ cx (String.fromFloat ball1.bx)
                    , cy (String.fromFloat ball1.by)
                    , r (String.fromFloat rball)
                    , fill "gold"
                    , attribute "filter" "url(#MyFilter)"
                    ]
                    []
            else if ball1.speed <= 1/2.8 * 1.5 then
                circle
                    [ cx (String.fromFloat ball1.bx)
                    , cy (String.fromFloat ball1.by)
                    , r (String.fromFloat rball)
                    , fill "orange"
                    , attribute "filter" "url(#MyFilter)"
                    ]
                    []
            else if ball1.speed <= 1/2.8 * 1.5 * 1.5 then
                circle
                    [ cx (String.fromFloat ball1.bx)
                    , cy (String.fromFloat ball1.by)
                    , r (String.fromFloat rball)
                    , fill "orangered"
                    , attribute "filter" "url(#MyFilter)"
                    ]
                    []
            else
                 circle
                     [ cx (String.fromFloat ball1.bx)
                     , cy (String.fromFloat ball1.by)
                     , r (String.fromFloat rball)
                     , fill "red"
                     , attribute "filter" "url(#MyFilter)"
                     ]
                     []
    in
    List.map (\ ball2 -> mapBall ball2) ball
        
renderMonster : Model -> List (Svg Msg)
renderMonster {monster} =
    let 
        mapmonster : Monster -> Svg Msg
        mapmonster monster1 =
            image
                [ xlinkHref "https://wx1.sbimg.cn/2020/06/19/mummy.png"
                , x (String.fromFloat (monster1.bx - monster1.rmonster))
                , y (String.fromFloat (monster1.by - monster1.rmonster))
                , width (String.fromFloat (monster1.rmonster * 2))
                , height (String.fromFloat (monster1.rmonster * 2))
                ]
                []
    in
    List.map (\ x -> mapmonster x) monster

renderLandMine : Model -> List (Svg Msg)
renderLandMine model =
    let
        mapLandMine : Landmine -> Svg Msg
        mapLandMine landmine1 =
            image
                [ xlinkHref "https://wx2.sbimg.cn/2020/06/19/spiderweb.png"
                , x (String.fromInt (landmine1.bx - 60))
                , y (String.fromInt (landmine1.by - 75))
                , width (String.fromFloat (model.bwid + 120))
                , height (String.fromFloat (model.bhei + 150))
                ]
                []
    in
    List.map (\x -> mapLandMine x) model.landmine

renderPaddle : Model -> List (Svg Msg)
renderPaddle { paddle, pwid, phei } =
    [ rect
        [ x (String.fromFloat paddle.px)
        , y (String.fromFloat paddle.py)
        , rx "5"
        , ry "5"
        , width (String.fromFloat pwid)
        , height (String.fromFloat phei)
        , fill "orange"
        , attribute "filter" "url(#MyFilter)"
        ]
        []
    ]

renderResult : Model -> String
renderResult model =
    if List.isEmpty (List.filter (\ ball1 -> ball1.by <= toFloat (model.coY + 800)) model.ball) then
        "Game Over!"
    else
        case model.mode of
            1 -> 
                ""
            2 ->
                if model.treasureNum >= (generateClassicLevel (Maybe.withDefault 1 model.classiclevel)).treasureNum then
                    "You win"
                else
                    ""
            _ ->
                if model.state == EndPage && model.score < (model.limitedTimeLevel + 1) * 1000 then
                    "Game Over!"
                else if model.state == EndPage && model.score >= (model.limitedTimeLevel + 1) * 1000 then
                    String.fromInt(model.score) ++ "points!"
                else
                    ""


renderLabel : Model -> List (Svg Msg)
renderLabel model =
    if List.member model.state [Paused, Playing, Countdown] then
        [ image
            [ xlinkHref "https://i.loli.net/2020/06/18/WCURey9gTAIruXG.png"
            , x "1760"
            , y "240"
            , width "450"
            , height "200"
            ]
            []
        , text_
            [ x "1830"
            , y "400"
            , fontSize "35"
            , fontStyle "normal"
            , fill "white"
            ]
            [ text (String.fromInt model.score)
            ]
        , if model.mode == 2 then
                rect [] []
          else
                image
                    [ xlinkHref "https://i.loli.net/2020/06/18/ky47uQLVtzDi9Cj.png"
                    , x "1760"
                    , y "420"
                    , width "450"
                    , height "200"
                    ]
                    []
        , if model.mode == 2 then
                rect [] []
          else
            text_
               [ x "1830"
               , y "610"
               , fontSize "35"
               , fontStyle "normal"
               , fill
               (if model.mode == 2 then
               "none"
               else
               "white")
               ]
               [ text (String.fromInt model.level)
               ]
               , if model.mode /= 3 then
                     rect[][]
                else
                     image
                     [ xlinkHref "https://i.loli.net/2020/06/19/8tbAxX9aio4cYJm.png"
                     , x "1765"
                     , y "570"
                     , width "450"
                     , height "200"
                     ]
                     []
                     , if model.mode /= 2 then
                           rect[][]
                       else
                           image
                           [ xlinkHref "https://i.loli.net/2020/06/19/gToUu4Pdvmsp8iJ.png"
                           , x "80"
                           , y "760"
                           , width "450"
                           , height "200"
                           ]
                           []

        ]
    else
        [rect [ x "0", y "0", width "1", height "1", fill "transparent"] []]

renderLevelButton : Model -> List (Svg Msg)
renderLevelButton { state,levelbutton,lwid,lhei,filt } =
    let
        mapButton : (LevelButton, lwid,lhei) -> Svg Msg
        mapButton (levelbutton1,m,n)  =
            let
                msg =
                    case levelbutton1.levelnum of
                        1 ->
                            StartM2L1
                        2 ->
                            StartM2L2
                        3 ->
                            StartM2L3
                        4 ->
                            StartM2L4
                        5 ->
                            StartM2L5
                        6 ->
                            StartM2L6
                        7 ->
                            StartM2L7
                        _ ->
                            StartM2L8

            in
             g  [ onClick msg
                , onMouseDown FilterOn
                , onMouseUp FilterOff
                ]
                [image
                    [ if filt == 0 then
                        xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood5.png"
                      else
                        xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood6.png"
                    , x (String.fromInt levelbutton1.lx)
                    , y (String.fromInt levelbutton1.ly)
                    , width "250"
                    , height "100"
                    ]
                    []
                ,text_
                    [ x (String.fromInt (levelbutton1.lx+55))
                    , y (String.fromInt (levelbutton1.ly+55))
                    , fontSize "28"
                    , fontStyle "normal"
                    , fill "brown"
                    ]
                    [text "LEVEL", text (String.fromInt levelbutton1.levelnum)]
                ,rect
                    [ x (String.fromInt levelbutton1.lx)
                    , y (String.fromInt levelbutton1.ly)
                    , width "250"
                    , height "100"
                    , fill "transparent"
                    ]
                    []
                ]
    in
    if  state == Model.Level then
            List.map (\ x -> mapButton (x,lwid,lhei)) levelbutton
    else
            [rect[][]]


renderGameButton : Model -> List(Svg Msg)
renderGameButton {state, mode, filt} =
    let
        msg  =
            case state of
                Model.Stopped ->
                    PageTwo

                Model.Playing ->
                    Pause

                Model.Paused ->
                    Resume

                Model.SecondPage ->
                    PageThree

                Model.ThirdPage ->
                    PageFour

                Model.FourthPage ->
                    StartModeOne

                Model.EndPage ->
                    StartAgain

                _->
                    Noop
        txt =
            case state of
                Model.Stopped ->
                    "Next"

                Model.Playing ->
                    "Pause"

                Model.Paused ->
                    "Resume"

                Model.SecondPage ->
                    "Next Page"

                Model.ThirdPage ->
                    "Next Page"

                Model.FourthPage ->
                    "Endless Mode"

                Model.EndPage ->
                    "Start Again!"

                _->
                    ""
    in
    if (mode == 3 && state == Playing) || state == Countdown || state == Level then
        [rect[][]]
    else
    [ g [ onClick msg
        , onMouseDown FilterOn
        , onMouseUp FilterOff
        ]
        [ image
              [ if filt == 0 then
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood5.png"
                else
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood6.png"
              , if txt == "Endless Mode" then
                    x "350"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    x "1750"
                else
                    x "1500"
              , if txt == "Endless Mode" then
                    y "720"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    y "750"
                else
                    y "575"
              , width "250"
              , height "100"
              ]
              []
        , text_
              [ if txt == "Endless Mode" then
                    x "400"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    x "1810"
                else
                    x "1560"
              , if txt == "Endless Mode" then
                    y "775"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    y "805"
                else
                    y "630"
              , fontSize "25"
              , fontStyle "normal"
              , fill "brown"
              ]
              [text
              (if (mode == 3 && state == Playing )|| state == Countdown then
                  ""
              else
                  txt)
              ]
        , rect
              [ if txt == "Endless Mode" then
                    x "350"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    x "1750"
                else
                    x "1500"
              , if txt == "Endless Mode" then
                    y "720"
                else if txt == "Pause" || txt == "Resume" || txt == "Star Again!" then
                    y "750"
                else
                    y "575"
              , width "250"
              , height "100"
              , fill "transparent"
              ]
              []
        ]
    ]


renderButton : Model -> List(Svg Msg)
renderButton {state, filt}=
    [ g [ onClick ChooseLevel
        , onMouseDown FilterOn
        , onMouseUp FilterOff
        , display
        (if state == Model.FourthPage then
              "block"
         else
              "none"
        )
        ]
        [ image
              [ if filt == 0 then
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood5.png"
                else
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood6.png"
              , x "830"
              , y "720"
              , width "250"
              , height "100"
              ]
              []
        , text_
              [ x "890"
              , y "775"
              , fontSize "25"
              , fontStyle "normal"
              , fill "brown"
              ]
              [ text "Classic Mode" ]
        , rect
             [ x "830"
             , y "720"
             , width "250"
             , height "100"
             , fill "transparent"
             ]
             []
        ]
    ]

renderTMButton : Model -> List(Svg Msg)
renderTMButton {state, filt}=
    [ g [ onClick StartModeThree
        , onMouseDown FilterOn
        , onMouseUp FilterOff
        , display
        (if state == Model.FourthPage then
              "block"
         else
              "none"
        )
        ]
        [ image
              [ if filt == 0 then
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood5.png"
                else
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood6.png"
              , x "1320"
              , y "720"
              , width "250"
              , height "100"
              ]
              []
        , text_
              [ x "1380"
              , y "775"
              , fontSize "25"
              , fontStyle "normal"
              , fill "brown"
              ]
              [ text "Time Mode" ]
        , rect
             [ x "1320"
             , y "720"
             , width "250"
             , height "100"
             , fill "transparent"
             ]
             []
        ]
    ]



renderPrePage : Model -> List(Svg Msg)
renderPrePage {state, filt} =
    let
        msg  =
            case state of
               Model.SecondPage ->
                    PageOne

               Model.ThirdPage ->
                    PageTwo

               Model.FourthPage ->
                    PageThree

               Model.EndPage ->
                    Exit
               _ ->
                    Noop
        txt =
            case state of
                Model.SecondPage ->
                     "Previous Page"

                Model.ThirdPage ->
                     "Previous Page"

                Model.FourthPage ->
                     "Previous Page"

                Model.EndPage ->
                     "Exit"
                _ ->
                     ""
    in
    [ g [ onClick msg
        , onMouseDown FilterOn
        , onMouseUp FilterOff
        , display
        (if state == Model.FourthPage || state == Model.ThirdPage || state == Model.SecondPage || state == Model.EndPage then
               "block"
         else
               "none"
        )
        ]
        [ image
              [ if filt == 0 then
                    xlinkHref "https://wx1.sbimg.cn/2020/06/19/wood8.png"
                else
                    xlinkHref "https://wx2.sbimg.cn/2020/06/19/wood7.png"
              , x "1500"
              , y "140"
              , width "250"
              , height "100"
              ]
              []
        , text_
              [ if txt == "Exit" then
                    x "1590"
                else
                    x "1540"
              , y "195"
              , fontSize "25"
              , fontStyle "normal"
              , fill "brown"
              ]
              [ text txt ]
        , rect
              [ x "1500"
              , y "140"
              , width "250"
              , height "100"
              , fill "transparent"
              ]
              []
        ]
    ]

renderInfoBox : Model -> List (Svg Msg)
renderInfoBox { coX, coY, state } =
    [ rect
          [ fill
             (if state == Stopped || state == SecondPage || state == ThirdPage || state == FourthPage || state == Level || state == EndPage then
                 "white"
             else
                 "none")
          , fillOpacity "0.6"
          , x "250"
          , y "100"
          , height "800"
          , width "1498"
          ]
          []
    ]
renderInfo1 : Model -> String
renderInfo1 model =
        if model.state == SecondPage then
            " Some things under certain bricks:"
        else if model.state == ThirdPage then
            " Some things under certain bricks:"
        else if model.state == FourthPage then
            " Choose a mode and play!"
        else
            ""

renderInfo3 : Model -> String
renderInfo3 model =
        if model.state == Stopped then
            "With an adventurous spirit, you decide to venture into the pyramids. "
        else
            ""

renderInfo4 : Model -> String
renderInfo4 model =
        if model.state == Stopped then
            " A voice whispers in your ear that the treasure is undoubtedly tempting,"
        else
            ""

renderInfo5 : Model -> String
renderInfo5 model =
        if model.state == Stopped then
            " but curiosity may also kill the cat..."
        else
            ""

renderPattern1 : Model -> List(Svg Msg)
renderPattern1 {state} =
    if state == SecondPage then
         [image
            [xlinkHref "https://wx2.sbimg.cn/2020/06/19/treasure.png"
            , x "410"
            , y "390"
            , width "230"
            , height "230"
            ]
            []
         ]
    else if state == ThirdPage then
        [image
            [xlinkHref "https://wx1.sbimg.cn/2020/06/19/addball.png"
            , x "410"
            , y "390"
            , width "230"
            , height "230"
            ]
            []
         ]
    else if state == FourthPage then
         [image
             [xlinkHref "https://wx1.sbimg.cn/2020/06/19/mode11.png"
             , x "300"
             , y "330"
             , width "400"
             , height "400"
             ]
             []
          ]
    else
           [ rect [][]]


renderPattern2 : Model -> List(Svg Msg)
renderPattern2 {state} =
    if state == SecondPage then
         [image
            [xlinkHref "https://wx2.sbimg.cn/2020/06/19/spiderweb.png"
            , x "742"
            , y "415"
            , width "230"
            , height "230"
            ]
            []
         ]
    else if state == ThirdPage then
        [image
            [xlinkHref "https://wx1.sbimg.cn/2020/06/19/mummy.png"
            , x "1020"
            , y "390"
            , width "230"
            , height "230"
            ]
            []
         ]
    else if state == FourthPage then
         [image
             [xlinkHref "https://wx1.sbimg.cn/2020/06/19/mode22.png"
             , x "780"
             , y "330"
             , width "400"
             , height "400"
             ]
             []
          ]
    else
           [ rect [][]]

renderPattern3 : Model -> List(Svg Msg)
renderPattern3 {state} =
    if state == SecondPage then
         [image
            [xlinkHref "https://wx2.sbimg.cn/2020/06/19/p.png"
            , x "1080"
            , y "390"
            , width "220"
            , height "220"
            ]
            []
         ]
    else if state == FourthPage then
         [image
            [xlinkHref "https://wx1.sbimg.cn/2020/06/19/mode33.png"
            , x "1260"
            , y "330"
            , width "400"
            , height "400"
            ]
            []
         ]
    else
           [ rect [][]]


view : Model -> Html Msg
view model =
  svg
    [ viewBox "0 0 2000 950"
    , preserveAspectRatio "xMidYMid meet"
    ]
    (  renderFilter
    ++ renderBackground
    ++ renderBox model
    ++ renderSpecial model
    ++ renderPreparingBrick model
    ++ renderBrick model
    ++ renderLandMine model
    ++ renderBall model
    ++ renderPaddle model
    ++ renderMonster model
    ++ renderBox_ model
    ++ renderInfoBox model
    ++ renderButton model
    ++ renderGameButton model
    ++ renderPrePage model
    ++ renderTMButton model
    ++ renderPattern1 model
    ++ renderPattern2 model
    ++ renderPattern3 model
    ++ renderLevelButton model
    ++ renderLabel model
    ++ [ text_
            [ x "780"
            , y "700"
            , fontSize "80"
            , fontStyle "normal"
            , fillOpacity "0.8"
            , fill "darkblue"
            ]
            [
            (if model.state == EndPage then
                text (renderResult model)
            else
                text "")
            ]
       ]
    ++ [ text_
            [ x "740"
            , y "230"
            , fontSize "80"
            , fontStyle "normal"
            , fillOpacity "0.7"
            , fill "white"
            , fontWeight "bold"
            ]
                (if model.mode == 3 && model.state == Countdown then
                   [ text "Time Limit: 40 s"]
                 else
                   [ text ""]
                )
       ]
    ++ [ text_
            [ x "730"
            , y "350"
            , fontSize "80"
            , fontStyle "normal"
            , fillOpacity "0.7"
            , fill "white"
            , fontWeight "bold"
            ]
                (if model.mode == 3 && model.state == Countdown then
                   [ text "Goal:"
                   , text (String.fromInt ((model.limitedTimeLevel+1) * 1000))
                   , text "points"
                   ]
                 else
                   [ text ""]
                )
       ]
    ++ [ text_
            [ x "610"
            , y "350"
            , fontSize "80"
            , fontStyle "normal"
            , fillOpacity "0.7"
            , fill "white"
            , fontWeight "bold"
            ]
                (if model.mode == 2 && (model.state == Countdown || model.state == Paused) then
                   [ text "Find all the treasures!"]
                 else
                   [ text ""]
                )
       ]
    ++ [ text_
            [ x "1810"
            , y "840"
            , fontSize "50"
            , fontStyle "normal"
            , fill "white"
            ]
                (if model.mode == 3 && model.state == Playing then
                   [ text (String.fromInt ((model.limitedTime - Time.posixToMillis model.time) // 1000))
                    , text ":"
                    , text (String.fromInt (modBy 1000 (model.limitedTime - Time.posixToMillis model.time) ))  ]
                 else
                   [ text ""]
                )
            ]
    ++ [ text_
            [ x "1810"
            , y "740"
            , fontSize "35"
            , fontStyle "normal"
            , fill "white"
            ]
                (if model.mode == 3 && (model.state == Playing || model.state == Countdown) then
                   [ text (String.fromInt ((model.limitedTimeLevel+1) * 1000)) ]
                 else
                   [ text ""]
                )
       ]
    ++ [ text_
            [ x "270"
            , y "200"
            , fontSize "60"
            , fontStyle "normal"
            , fill "brown"
            ]
            [ text (renderInfo1 model)
            ]
       ]
   ++ [text_
            [ x "455"
            , y "320"
            , fontSize "37"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " Treasure"]
                        --: You can get additional bonus if you find the treasures! "
                    else if model.state == ThirdPage then
                      [text  " Bonus Ball"]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "305"
            , y "700"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " You can get additional bonus"
                      ]
                    else if model.state == ThirdPage then
                      [text  "Some bricks contain bonus balls which  "]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "675"
            , y "700"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  "Traps cause collapses within"]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "670"
            , y "760"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " a certain radius which destory "]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "675"
            , y "820"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " both bricks and balls."]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "1045"
            , y "700"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  "Some mysterious poison that"]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "1045"
            , y "760"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " will speed up the ball sharply!"]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "310"
            , y "760"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  "if you find the treasures!"
                      ]
                    else if model.state == ThirdPage then
                      [text  "help you to eliminate bricks more quickly."]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "875"
            , y "700"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == ThirdPage then
                      [ text  " Mummy is so angry at what has what has  disturbed "]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "895"
            , y "760"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == ThirdPage then
                      [ text  " his sleep that he will not disappear until"]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "895"
            , y "820"
            , fontSize "25"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == ThirdPage then
                      [ text  "he destroys three balls in the area."]
                    else
                       [text ""]
                    )
       ]
   ++ [text_
            [ x "805"
            , y "320"
            , fontSize "37"
            , fontStyle "normal"
            , fill "brown"
            ]
                    (if model.state == SecondPage then
                      [ text  " Traps"]
                    else
                        [text ""]
                    )
         ]
   ++ [text_
        [ x "1080"
        , y "320"
        , fontSize "37"
        , fontStyle "normal"
        , fill "brown"
        ]
        (if model.state == ThirdPage then
             [text  " Mummy"]
         else
             [text  ""])

        ]
   ++ [text_
        [ x "1130"
        , y "320"
        , fontSize "37"
        , fontStyle "normal"
        , fill "brown"
        ]
        (if model.state == SecondPage then
             [text  " Poison"]
         else
             [text  ""])

        ]
   ++ [ text_
            [ x "385"
            , y "400"
            , fontSize "31"
            , fontStyle "normal"
            , fill "brown"
            ]
            [ text (renderInfo3 model)
            ]
       ]
   ++ [ text_
            [ x "385"
            , y "500"
            , fontSize "31"
            , fontStyle "normal"
            , fill "brown"
            , fillOpacity "0.7"
            ]
            [ text (renderInfo4 model)
            ]
       ]
   ++ [ text_
            [ x "385"
            , y "600"
            , fontSize "31"
            , fontStyle "normal"
            , fill "brown"
            , fillOpacity "0.5"
            ]
            [ text (renderInfo5 model)
            ]
       ]
   ++ [ text_
            [ x "960"
            , y "610"
            , fontSize "270"
            , fontStyle "normal"
            , fill "darkblue"
            , fillOpacity "0.5"
            , fontWeight "bold"
            ]
            (if model.state == Countdown then
                [text (String.fromInt ((model.countdownLimit - Time.posixToMillis model.time) // 1000))]
            else
                [text ""]

            )
       ]

   ++[ text_
            [ x "730"
            , y "530"
            , fontSize "80"
            , fontStyle "normal"
            , fillOpacity "0.7"
            , fill "darkblue"
            , fontWeight "bold"
            ]
                (if model.state == EndPage then
                   [ text "Your Score: "
                   , text (String.fromInt model.score)]
                 else
                   [ text ""]
                )
       ]
   ++[ text_
            [ x "550"
            , y "330"
            , fontSize "70"
            , fontStyle "normal"
            , fillOpacity "0.5"
            , fill "darkblue"
            , fontWeight "bold"
            ]
                (if model.state == EndPage then
                   [ text "You find  "
                   , text (String.fromInt model.treasureNum)
                   , text " pile(s) of treasure."
                   ]
                 else
                   [ text ""]
                )
       ]
    ++ renderCover model
    ++ [ if model.state /= Stopped then
            rect[][]
        else
            image
                     [ xlinkHref "https://wx1.sbimg.cn/2020/06/19/title.png"
                     , x "0"
                     , y "5"
                     , width "2400"
                     , height "500"
                     ]
                     []
       ]
    ++  case model.mode of
           2 ->
             (if model.state == Stopped || model.state == Level then
                   [text_[][]]
              else
               [ text_
                   [ x "290"
                   , y "920"
                   ,fontSize "40"
                   ,fontStyle "normal"
                   ,fill "white"
                   ]

                   [ text (String.fromInt model.treasureNum)
                   , text "/"
                   , text (String.fromInt (generateClassicLevel (Maybe.withDefault 1 model.classiclevel)).treasureNum)
                   ]
               ]
               )
           _ ->
               [ text_ [][]]


    )

