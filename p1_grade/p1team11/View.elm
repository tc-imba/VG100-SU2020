module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autoplay, loop, src, style)
import Html.Events exposing (on, onClick)
import Messages exposing (Msg(..))
import Model exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (height, width)


viewWindow : Window -> Svg msg
viewWindow window =
    Svg.rect
        [ Svg.Attributes.fill <| window.background
        , Svg.Attributes.x <| String.fromFloat window.x
        , Svg.Attributes.y <| String.fromFloat window.y
        , Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat window.height
        , Svg.Attributes.stroke "#2b92a3"
        , Svg.Attributes.strokeWidth "1"
        ]
        []


viewPaddle : Paddle -> Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill <| paddle.background
        , Svg.Attributes.x <| String.fromFloat paddle.x
        , Svg.Attributes.y <| String.fromFloat paddle.y
        , Svg.Attributes.width <| String.fromFloat paddle.width
        , Svg.Attributes.height <| String.fromFloat paddle.height
        ]
        []


viewBall : Ball -> Svg msg
viewBall ball =
    Svg.circle
        [ Svg.Attributes.fill <| ball.background
        , Svg.Attributes.cx <| String.fromFloat ball.cx
        , Svg.Attributes.cy <| String.fromFloat ball.cy
        , Svg.Attributes.r <| String.fromFloat ball.radius
        ]
        []



{-
   viewBlock : Block -> Svg msg
   viewBlock block =
       Svg.rect
           [ Svg.Attributes.fill <| block.background
           , Svg.Attributes.x <| String.fromFloat block.x
           , Svg.Attributes.y <| String.fromFloat block.y
           , Svg.Attributes.width <| String.fromFloat block.width
           , Svg.Attributes.height <| String.fromFloat block.height
           , Svg.Attributes.stroke <| block.stroke
           , Svg.Attributes.strokeWidth <| String.fromFloat block.strokeWidth
           ]
           []
-}


renderGameMode : Html Msg
renderGameMode =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "bottom" "300px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "640px"
        , style "top" "600px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "200px"
        , onClick GameMode
        ]
        [ text "Game Mode" ]


--renderScreenSetting : Html Msg
--renderScreenSetting =
--    button
--        [ style "background" "#000000"
--        , style "border" "20"
--        , style "bottom" "225px"
--        , style "color" "#018fce"
--        , style "cursor" "pointer"
--        , style "display" "block"
--        , style "font-family" "Helvetica, Arial, sans-serif"
--        , style "font-size" "18px"
--        , style "font-weight" "300"
--        , style "height" "60px"
--        , style "left" "640px"
--        , style "top" "680px"
--        , style "line-height" "60px"
--        , style "outline" "none"
--        , style "padding" "0"
--        , style "position" "absolute"
--        , style "width" "200px"
--        , onClick ScreenSize
--        ]
--        [ text "Screen Setting" ]


renderHelp : Html Msg
renderHelp =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "bottom" "225px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "640px"
        , style "top" "680px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "200px"
        , onClick Help
        ]
        [ text "Help" ]


--renderScreenSizeButton : ScreenSize -> Html Msg
--renderScreenSizeButton size =
--    let
--        ( msg, bottomPosition, txt ) =
--            case size of
--                Small ->
--                    ( SmallScreen, 600, "Small" )

--                Medium ->
--                    ( MediumScreen, 680, "Medium" )

--                Large ->
--                    ( LargeScreen, 760, "Large" )
--    in
--    button
--        [ style "background" "#000000"
--        , style "border" "20"
--        , style "top" (String.fromFloat bottomPosition ++ "px")
--        , style "color" "#018fce"
--        , style "cursor" "pointer"
--        , style "display" "block"
--        , style "font-family" "Helvetica, Arial, sans-serif"
--        , style "font-size" "18px"
--        , style "font-weight" "300"
--        , style "height" "60px"
--        , style "left" "640px"
--        , style "line-height" "60px"
--        , style "outline" "none"
--        , style "padding" "0"
--        , style "position" "absolute"
--        , style "width" "200px"
--        , onClick msg
--        ]
--        [ text txt ]


renderGameModeButton : Mode -> Html Msg
renderGameModeButton gameMode =
    let
        ( msg, bottomPosition, txt ) =
            case gameMode of
                Story ->
                    ( StoryMode, 680, "Story Mode" )

                Random ->
                    ( OrdinaryMode, 600, "Ordinary Mode" )
    in
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "top" (String.fromFloat bottomPosition ++ "px")
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "640px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "200px"
        , onClick msg
        ]
        [ text txt ]


renderBackButton : Msg -> Html Msg
renderBackButton previousMsg =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "top" "5px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "8px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "80px"
        , onClick previousMsg
        ]
        [ text "Back" ]

------------
viewProp : Prop -> Html msg
viewProp prop =
    let
        website =
               if prop.kind == "bigger" then
                    "https://www.z4a.net/images/2020/06/18/prop1db4e542d88451af2.png"
               else
                    "https://www.z4a.net/images/2020/06/18/prop24c93b2d2c0eef808.png"
    in
    img
        [ src website
        , style "left" <| String.fromFloat prop.cx ++ "px"
        , style "top" <| String.fromFloat prop.cy ++ "px"
        , style "position" "absolute"
        ]
        []

viewBlock0 : Block -> Html msg
viewBlock0 block =
    img
        [ src "http://r.photo.store.qq.com/psc?/V14EmBeb3D256g/Tiy10*PRF5enyWrLfdcKz7dRtyVoBogmvtUlaV7yzWa7SD9DjmFmbsHq1zqFd5Z3Dao6wkJ.BO6EmiTdCkxngxCi0Yr0TqZ54EroAqEj8Os!/r"
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlock1 : Block -> Html msg
viewBlock1 block =
    img
        [ src "http://r.photo.store.qq.com/psc?/V14EmBeb3D256g/Tiy10*PRF5enyWrLfdcKz0rAuc.FGosXHS4imSc1BsZEzCxnKrX*lWL.ONtIkqpXg7h9miWvUShKbWOATHUjt8WxAytP4Gs4H75I.Lajj8A!/r"
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlock2 : Block -> Html msg
viewBlock2 block =
    img
        [ src "http://r.photo.store.qq.com/psc?/V14EmBeb3D256g/Tiy10*PRF5enyWrLfdcKz*PtPQliB*xbk1Ztd.Pstmo7OavIBo6Z.5T7Q50qrFfxilnDAXOmkzGp0tBWKY.RVa*s9kEwoLxmW55lMznLYEk!/r"
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlockAccelerate : Block -> Html msg
viewBlockAccelerate block =
    img
        [ src "https://ae01.alicdn.com/kf/Hff708fd4b9104bb4abace6f432380fb9w.jpg" -- accelerate.png
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlockDecelerate : Block -> Html msg
viewBlockDecelerate block =
    img
        [ src "https://ae01.alicdn.com/kf/H64077ee158934e6b8644ff714699b0c9p.jpg" --decelerate.png
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlockImmortal2 : Block -> Html msg
viewBlockImmortal2 block =
    img
        [ src "https://ae01.alicdn.com/kf/Ha14929c3ffde46c1967a32113fc38dbcQ.jpg" -- immortal2.png
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlockImmortal1 : Block -> Html msg
viewBlockImmortal1 block =
    img
        [ src "https://ae01.alicdn.com/kf/He30537b4b9de460bab52cae74bf48864Q.jpg" -- immortal1.png
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewBlockStageClear : Block -> Html msg
viewBlockStageClear block =
    img
        [ src "https://ae01.alicdn.com/kf/H69a74a1a219345faaa88060ca88d9ad5f.jpg"
        , style "left" <| String.fromFloat block.x ++ "px"
        , style "top" <| String.fromFloat block.y ++ "px"
        , style "position" "absolute"
        ]
        []


viewDescription : String -> Html msg
viewDescription txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "570px"
        , style "top" "400px"
        , style "position" "absolute"
        ]
        [ text txt ]


viewDescription2 : String -> Html msg
viewDescription2 txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "620px"
        , style "top" "450px"
        , style "position" "absolute"
        ]
        [ text txt ]


viewDescription3 : String -> Html msg
viewDescription3 txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "500px"
        , style "top" "400px"
        , style "position" "absolute"
        ]
        [ text txt ]


viewDescription4 : String -> Html msg
viewDescription4 txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "495px"
        , style "top" "450px"
        , style "position" "absolute"
        ]
        [ text txt ]


renderRestartButton : Html Msg
renderRestartButton =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "top" "550px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "right" "510px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "100px"
        , onClick Restart
        ]
        [ text "Restart" ]


renderStartButton : Html Msg
renderStartButton =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "bottom" "100px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "right" "400px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "100px"
        , onClick Story1
        ]
        [ text "Start" ]


renderJumpButton : Html Msg
renderJumpButton =
    button
        [ style "background" "#000000"
        , style "border" "20"
        , style "bottom" "100px"
        , style "color" "#018fce"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "right" "510px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "300px"
        , onClick Jump
        ]
        [ text "Jump to the next stage" ]


renderHelpInfo : Html msg
renderHelpInfo =
    div
        [ style "color" "#018fce"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "500px"
        , style "top" "200px"
        , style "position" "absolute"
        ]
        [ text "Shift: start; Enter: reset; ←: move left; →: move right" ]


viewDescription5 : String -> Html msg
viewDescription5 txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "30px"
        , style "line-height" "0px"
        , style "margin" "30px 0 0"
        , style "right" "570px"
        , style "top" "500px"
        , style "position" "absolute"
        ]
        [ text txt ]


cube =
    img
        [ style "top" "350px"
        , style "left" "650px"
        , style "position" "absolute"
        , src "https://jbox.sjtu.edu.cn:10081/thumb/files/databox/?delivery_code=0d37463e99154516978454610ae8c8ef&delivery_token=eyJwd2QiOiIiLCJleHBpcmF0aW9uIjoxNTkyMjg5OTk2Njc1LCJjb2RlIjoiMGQzNzQ2M2U5OTE1NDUxNjk3ODQ1NDYxMGFlOGM4ZWYiLCJtb2RlIjoicnAifQ==&preview_type=origin&user_op=preview"
        ]
        []


bgm =
    div []
        [ audio
            [ src "http://downsc.chinaz.net/Files/DownLoad/sound1/201812/10909.mp3"
            , autoplay True
            , loop True
            ]
            []
        ]


bgm2 =
    div []
        [ audio
            [ src "http://downsc.chinaz.net/Files/DownLoad/sound1/201808/10460.mp3"
            , autoplay True
            , loop True
            ]
            []
        ]


sound =
    div []
        [ audio
            [ src "http://downsc.chinaz.net/Files/DownLoad/sound1/201905/11479.mp3"
            , autoplay True
            ]
            []
        ]



-- Drawing different elements


viewSingleBlock : Block -> Html Msg
viewSingleBlock block =
    case block.blocktype of
        BrickVertex ->
            viewBlock0 block

        BrickBrown ->
            viewBlock1 block

        BrickWhite ->
            viewBlock2 block

        Accelerate ->
            viewBlockAccelerate block

        Decelerate ->
            viewBlockDecelerate block

        Immortal1 ->
            viewBlockImmortal1 block

        Immortal2 ->
            viewBlockImmortal2 block

        _ ->
            viewBlockStageClear block


storyGeneralBoard : Model -> String -> Html Msg
storyGeneralBoard model url =
    let
        buttons =
            if model.state == Stopped && model.stage < 2 then
                [ renderBackButton Beginning
                , renderJumpButton
                , renderStartButton
                ]

            else if model.state == Stopped && model.stage == 4 then
                [ renderBackButton Beginning ]

            else if model.state == Stopped then
                [ renderBackButton Beginning
                , renderStartButton
                ]

            else if model.state == Lose then
                [ renderBackButton Beginning
                , renderRestartButton
                ]

            else
                []
    in
    div
        [ style "font-size" "30px"
        , style "background-color" "#000000"
        , style "background-image" ("url(\"" ++ url ++ "\")")
        , style "background-size" "contain"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center"
        ]
        ([ Svg.svg
            [ width (String.fromFloat (model.window.width + model.window.x))
            , height (String.fromFloat (model.window.height + 3 * model.window.y))
            ]
            []
         ]
            ++ buttons
        )


storyPlayingBoard : Model -> Html Msg
storyPlayingBoard model =
    let
        block =
            List.map viewSingleBlock model.block

        elements =
            [ {- viewWindow model.window, -} viewPaddle model.paddle, viewBall model.ball ]
    in
    div
        [ style "font-size" "30px"
        , style "background-color" "#171515"
        , style "background-image" "url(\"https://ae01.alicdn.com/kf/H56e74eefa3e04117a9a87efd7b8cc672R.jpg\")"
        , style "background-size" "contain"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center"
        ]
        ([ Svg.svg
            [ width (String.fromFloat (model.window.width + model.window.x))
            , height (String.fromFloat (model.window.height + 3 * model.window.y))
            ]
            (List.concat [ elements ])
         ]
            ++ [viewProp model.prop]
            ++ [ cube ]
            ++ [ bgm, bgm2 ]
            ++ block
            ++ [ sound ]
        )


view : Model -> Html Msg
view model =
    let
        block =
            List.map viewSingleBlock model.block

        elements =
            [ {- viewWindow model.window, -} viewPaddle model.paddle, viewBall model.ball ]
    in
    case model.mode of
        Random ->
            case model.state of
                Stopped ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://ae01.alicdn.com/kf/H56e74eefa3e04117a9a87efd7b8cc672R.jpg\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                            ]
                            (List.concat [ elements ])

                         {-
                            , viewDescription "Press Shift to start game!"
                            , viewDescription2 "Press Enter to reset."
                            , viewDescription5 "Press 1 to enter story mode."
                         -}
                         ]
                            ++ [ cube ]
                            ++ block
                            ++ [ bgm, bgm2 ]
                        )

                Playing ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://ae01.alicdn.com/kf/H56e74eefa3e04117a9a87efd7b8cc672R.jpg\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                            ]
                            (List.concat [ elements ])
                         ]
                            ++ [viewProp model.prop]
                            ++ [ cube ]

                            ++ [ bgm, bgm2 ]
                            ++ block
                            ++ [ sound ]
                        )

                Lose ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://ae01.alicdn.com/kf/H56e74eefa3e04117a9a87efd7b8cc672R.jpg\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ cube ]
                            ++ [ bgm, bgm2 ]
                            ++ block
                            ++ [ Svg.svg
                                    [ width (String.fromFloat (model.window.width + model.window.x))
                                    , height (String.fromFloat (model.window.height + 3 * model.window.y))
                                    ]
                                    (List.concat [ elements ])
                               , viewDescription3 "You lose!"
                               , viewDescription4 "Try again."
                               , renderRestartButton
                               , renderBackButton Beginning
                               ]
                        )

                Win ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://ae01.alicdn.com/kf/H56e74eefa3e04117a9a87efd7b8cc672R.jpg\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        [ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + model.window.y))
                            ]
                            (List.concat [ elements, block ])
                        , viewDescription3 "You win!"
                        , renderRestartButton
                        ]

                StartScreen ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#000000"
                        , style "background-image" "url(\"https://www.z4a.net/images/2020/06/18/2.gif\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                            , Svg.Attributes.fill "#000000"
                            ]
                            [ Svg.circle [] [] ]
                         ]
                            ++ [ renderGameMode
                               --, renderScreenSetting
                               , renderHelp
                               ]
                            ++ [ bgm, bgm2 ]
                        )

                ModeSetting ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://www.z4a.net/images/2020/06/18/2.gif\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                            , Svg.Attributes.fill "#000000"
                            ]
                            [ Svg.circle [] [] ]
                         ]
                            ++ [ renderGameModeButton Story
                               , renderGameModeButton Random
                               , renderBackButton Beginning
                               ]
                            ++ [ bgm, bgm2 ]
                        )

                --ScreenSetting ->
                --    div
                --        [ style "font-size" "30px"
                --        , style "background-color" "#171515"
                --        , style "background-image" "url(\"https://www.z4a.net/images/2020/06/18/2.gif\")"
                --        , style "background-size" "contain"
                --        , style "background-repeat" "no-repeat"
                --        , style "background-position" "center"
                --        ]
                --        ([ Svg.svg
                --            [ width (String.fromFloat (model.window.width + model.window.x))
                --            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                --            , Svg.Attributes.fill "#000000"
                --            ]
                --            [ Svg.circle [] [] ]
                --         ]
                --            ++ [ renderScreenSizeButton Small
                --               , renderScreenSizeButton Medium
                --               , renderScreenSizeButton Large
                               --, renderBackButton Beginning
                --               ]
                --            ++ [ bgm, bgm2 ]
                --        )

                HelpScreen ->
                    div
                        [ style "font-size" "30px"
                        , style "background-color" "#171515"
                        , style "background-image" "url(\"https://www.z4a.net/images/2020/06/18/2.gif\")"
                        , style "background-size" "contain"
                        , style "background-repeat" "no-repeat"
                        , style "background-position" "center"
                        ]
                        ([ Svg.svg
                            [ width (String.fromFloat (model.window.width + model.window.x))
                            , height (String.fromFloat (model.window.height + 3 * model.window.y))
                            , Svg.Attributes.fill "#000000"
                            ]
                            [ Svg.circle [] [] ]
                         ]++
                            [ bgm, bgm2 ]
                            ++ [ renderBackButton Beginning
                               , renderHelpInfo
                               ]
                        )

        Story ->
            case model.stage of
                0 ->
                    case model.state of
                        Stopped ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/Ha1cc4e9b48d24a81b47a071e56b985bcO.jpg"

                        --story1.png
                        Playing ->
                            storyPlayingBoard model

                        Lose ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/Hccaac54990db451ca06e76f1e7c572d3B.jpg"

                        --storyfail1.png
                        _ ->
                            div [] []

                1 ->
                    case model.state of
                        Stopped ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/Hb3f5eef0c0c7415d9a18c3d8ad2b09f18.jpg"

                        --story2.png
                        Playing ->
                            storyPlayingBoard model

                        Lose ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/Hccaac54990db451ca06e76f1e7c572d3B.jpg"

                        --storyfail1.png
                        _ ->
                            div [] []

                2 ->
                    case model.state of
                        Stopped ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/Hfa7d3f6801f2484790a13be83fb66232C.jpg"

                        --story3.png
                        Playing ->
                            storyPlayingBoard model

                        Lose ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/H7fa2815e938a4f46aa16874e622e0127P.jpg"

                        --storyfail2.png
                        _ ->
                            div [] []

                3 ->
                    case model.state of
                        Stopped ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/H4c6b92cbf0bd4fc1b7c44d0417c35bfc4.jpg"

                        --story4.png
                        Playing ->
                            storyPlayingBoard model

                        Lose ->
                            storyGeneralBoard model "https://ae01.alicdn.com/kf/H05bd906bd0994f54a2403572a98dae9fA.jpg"

                        --storyfail3.png
                        _ ->
                            div [] []

                4 ->
                    storyGeneralBoard model "https://ae01.alicdn.com/kf/H7a4b70a85ac0487aba65f5c0c74f3126U.jpg"

                --storyending.png
                _ ->
                    div [] []



--Combine the elements together
