module View exposing (view)



import Html exposing (Attribute, Html, audio, button, div, img, text, video)
import Html.Attributes exposing (attribute, autoplay, controls, href, loop, src, style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, circle, image, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, opacity, preserveAspectRatio, r, stroke, strokeWidth, transform, width, x, xlinkHref, y)
import Markdown exposing (toHtml)

import Model exposing (Model, State(..),ShoppingStatus(..))
import Entity exposing (Rect,Block, BlockType(..), Circle, Paddle, getLeftTop)
import Color exposing (Color)
import Message exposing (Msg(..), Buff(..))
import Model exposing (ShoppingStatus)

view: Model -> Html Msg
view model =
    div
    [ style "width" "100%"
    , style "height" "100%"
     ,style "position" "absolute"
    , style "background-image" "url(resources/Background.jpg)"
    , style "background-size" "100% 100%"
    --, style "background-attachment" "fixed"
    ]
    [
     case model.status of
       FirstPage ->
            renderFirstPage model
       Manual _ ->
            renderMaunalPage model
       Reference _ ->
            renderMaunalPage model
       _ ->
            renderGamePage model
    ]


renderMainButton: Model -> Html Msg
renderMainButton model =
    case model.status of
        Playing ->
            generateButton "Pause" Pause
        Stopped ->
            if model.hasStarted then
                generateButton "Try again" Start
            else
                generateButton "New Game" Start
        Paused ->
            generateButton "Resume" Resume
        _ ->
            div [] []

renderHomeButton: Html Msg
renderHomeButton =
    div
    [ style "height" "60px"
    , style "top" "500px"
    , style "line-height" "60px"
    , style "outline" "none"
    , style "padding" "0"
    , style "position" "absolute"
    ]
    [ generateButton "Home" OpenFirstPage]

rendermusic : String -> Html Msg
rendermusic name =
        video
        [ controls True
        , src  name
        , autoplay True
        , loop True
        ]
        []



blockToSvg: String -> Block ->  Svg Msg
blockToSvg image b =
   -- rectToSvg b.rect b.color
   addRectBackground b.rect image


generateButton : String -> Msg -> Html Msg
generateButton txt msg =
    button
    [ style "height" "60px"
    , style "width" "100px"
    , style "line-height" "60px"
    , style "outline" "none"
    , style "padding" "0"
    , style "border-style" "groove"
    , style "border-color" "black"
    , style "border-width" "2px"
    , style "position" "absolute"
    , style "box-shadow" "-1px -2px -2px 0 moccasin"
    , style "background-color" "grey"
    , style "font-size" "16px"
    , style "border-radius" "6px"
    , style "align" "center"
    , onClick msg]
    [text txt]


paddleToSvg : Paddle -> Html Msg
paddleToSvg p =
    --rectToSvg p.rect p.color
        addRectBackground p.rect "resources/paddle2.png"


rectToSvg : Rect -> Color -> Html Msg
rectToSvg r color =
    Svg.rect
    [ x ( r |> getLeftTop
            |> Tuple.first
            |> String.fromFloat
        )
    , y ( r |> getLeftTop
            |> Tuple.second
            |> String.fromFloat
        )
    , width (String.fromInt r.width)
    , height (String.fromInt r.height)
    , fill (Color.toString color)
    ]
    []

addRectBackground : Rect -> String ->  Svg Msg
addRectBackground r image  =
    Svg.image
        [ x ( r |> getLeftTop
                |> Tuple.first
                |> String.fromFloat
            )
        , y ( r |> getLeftTop
                |> Tuple.second
                |> String.fromFloat
            )
        , width (String.fromInt r.width)
        , height (String.fromInt r.height)
        , xlinkHref image
        ]
        []


circleToSvg: String -> Circle -> Svg Msg
circleToSvg image c=
    {-
    circle
    [ cx (String.fromInt (round (Tuple.first c.pos)))
    , cy (String.fromInt (round (Tuple.second c.pos)))
    , r (String.fromFloat c.radius)
    , fill (Color.toString c.color)
    ]
    []
    -}

    Svg.image
    [ x (String.fromInt (round (Tuple.first c.pos)))
    , y (String.fromInt (round (Tuple.second c.pos)))
    --, r (String.fromFloat c.radius)
    , width (String.fromFloat (2 * c.radius))
    , height (String.fromFloat (2 * c.radius))
    , xlinkHref image

    --, fill (Color.toString c.color)
    ]
    []
    {-
    Svg.pattern
        [ id "circleFill"
        , width "1"
        , height "1"
        , patternContentUnits "objectBoundingBox"
        ]
        [ Svg.image
            [ width "1"
            , height "1"
            , xlinkHref "ball.jpg"
            ]
            []
        , Svg.circle
        [ cx (String.fromInt (round (Tuple.first c.pos)))
        , cy (String.fromInt (round (Tuple.second c.pos)))
        , r (String.fromFloat c.radius)
        , fill "url(#circleFill)"
        ]
        []
        ]
    -}

renderLabel : String -> Html Msg
renderLabel txt =
    div
        [ style "color" "#0e0f10f8"
        , style "font-weight" "500"
        , style "font-size" "20px"
        , style "line-height" "1"
        , style "margin" "20px 0 0"
        , style "align" "center"
        , style "left" "200px"
        , style "font" "Consolas"
        ]
        [ text txt ]

renderTxt : String -> Html Msg
renderTxt txt =
        div
            [ style "color" "#0e0f10f8"
            , style "font-weight" "400"
            , style "font-size" "15px"
            , style "line-height" "20px"
            , style "margin" "10px 0 0"
            , style "left" "100px"
            ]
            [ text txt ]

renderCount : Int -> Html Msg
renderCount num =
    div
        [ style "color" "#0e0f10f8"
        , style "font-weight" "300"
        , style "line-height" "1"
        , style "margin" "10px 0 0"
        , style "align" "center"
        , style "white-space" "nowarp"
        ]
        [ text (String.fromInt num) ]

renderStoreButton: State -> Html Msg
renderStoreButton state =
    case state of
        Store _ ->
            generateButton "Back" LeaveStore
        _ ->
            generateButton "Witch's Hut" ArriveStore

renderStartButton : Model -> Html Msg
renderStartButton model =
    if model.hasStarted == False then
        generateButton "Start Game" OpenGamePage
    else
        generateButton "Resume" OpenGamePage

renderGamePanel : Model -> Html Msg
renderGamePanel model =
    div
        [ style "background" "#9AC0CD"
        , style "opacity" "0.9"
        , style "bottom" "30px"
        , style "color" "#34495f"
        , style "font-size" "15px"
        , style "border-style" "inset"
        , style "border-color" "#EBA04B"
        , style "border-width" "5px"
        , style "left" "1210px" --define the x coordinate of the panel
        , style "padding" "0 100px"
        , style "position" "absolute"   
        , style "right" "-400"
        , style "top" "50px"
        ]
        [ renderLabel ("Level " ++ String.fromInt model.stage)
        , renderLabel "Magic Points"
        , renderCount model.score
        , renderLabel "Required Magic"
        , renderCount model.requiredScore
        , renderTxt ("You have " ++ String.fromInt model.life ++ " magic balls left.")
        -- Store
        , div
            [ style "margin-top" "60px" --set the margin between buttons
            , style "position" "absolute"
            , style "left" "110px"
            ]
            [renderStoreButton model.status]
        -- Game
        , div
            [ style "margin-top" "140px" --set the margin between buttons
            , style "position" "absolute"
            , style "left" "110px"
            ]
            [renderMainButton model]
        ,if model.score >= model.requiredScore then
             div
                [ style "margin-top" "220px" --set the margin between buttons
                , style "position" "absolute"
                , style "left" "110px"
                ]
                [generateButton "Next Stage" NextStage]
        else
             div
                [ style "margin-top" "220px" --set the margin between buttons
                , style "position" "absolute"
                , style "left" "110px"
                ]
                [generateButton "Restart" Restart]
        , div
            [ style "margin-top" "300px" --set the margin between buttons
            , style "position" "absolute"
            , style "left" "110px"
            ]
            [generateButton "Home" OpenFirstPage]

        ,  if model.status == Playing then
            div
            [ style "margin" "180px 0 0 -480px"
            , style "position" "absolute"
            , style "visibility" "hidden"
            ]
            [rendermusic "resources/bgm.mp3"]
            else
            div [][]
        ]

renderStartPanel : Model -> Html Msg
renderStartPanel model =
    div
        [ style "bottom" "80px"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "14px"
        , style "left" "0px" --define the x coordinate of the panel
        , style "border-style" "solid"
        , style "border-radius" "10px"
        , style "padding" "0 100px"
        , style "position" "absolute"
        , style "right" "0"
        , style "top" "0"

        ]
        [div
            [ style "font-family" "lucida sans unicode,lucida grande, sans-serif"
            , style "color" "#15C2ED"
            , style "font-size" "40px"
            , style "font-weight" "bold"
            , style "font-style" "italic"
            , style "text-align" "center"
            , style "letter-spacing" "6.8pt"
            , style "word-spacing" "3pt"
            , style "margin-top" "30px"
            ]
            [ text "  Treasure Hunt  " ]
        ,   div
            [ style "margin-top" "60px" --set the margin between the label and the button
            , style "position" "absolute"
            , style "left" "700px"
             ]
            [renderStartButton model]
        , div
            [ style "margin-top" "140px" --set the margin between the label and the button
            , style "position" "absolute"
            , style "left" "700px"
            ]
           [generateButton "Help" OpenManual]
        , div
            [ style "margin-top" "220px" --set the margin between the label and the button
            , style "position" "absolute"
            , style "left" "700px"
            ]
            [generateButton "References" OpenReference]
        ]

renderGame: Model -> Html Msg

renderGame model =
    let
        background = [  rect
                        [ width "100%"
                        , height "100%"
                        , fill "lightblue"
                        , stroke "white"
                        , strokeWidth "5"
                        , opacity "0.1"
                        ]
                        []
                    ]
        blocks = blockListBackground model.blocks
        paddle = [paddleToSvg model.paddle]
        ball = ballListBackground model.balls "resources/magic1.png"
        coins = List.map (circleToSvg "resources/coin.png")model.coins
        bonusBalls = List.map (circleToSvg "resources/bonus2.png") model.bonusBalls
    in
    div
        [ style "left" "-390px" --define the x coordinate of the panel
        , style "position" "absolute"
        , style "right" "0"
        , style "top" "50px"
        ]
    [ svg
        [ width (String.fromInt model.width)
        , height (String.fromInt model.height)
        , transform "translate(400)"
        ]
        ( background ++ blocks ++ paddle ++ ball ++ coins ++ bonusBalls)
    ]

renderInfo : Model.State -> Html Msg
renderInfo state =
    div
        [ style "background" "rgba(236, 240, 241, 0.6)"
        , style "height" "700px"
        , style "left" "200px"
        , style "padding" "10px 0 0 50px"
        , style "position" "relative"
        , style "top" "20px"
        , style "width" "1100px"
        , style "display"
            (case state of
                Manual _ ->
                    "block"
                Reference _ ->
                    "block"
                _ ->
                    "none"
            )
        ]
        [case state of
                Manual _ ->
                     Markdown.toHtml [] """
#### 1. Brief Introduction & Victory Requirement
Treasure hunter is a game in which player breaks bricks with a bouncy ball to gain magic power, and pass the stage upon your magic level reaching the required amount of magic. Each stage will have a specified requirement. Magic can be used to strengthen the paddle and ball.
The amount of balls you control is limited. If you have lost all of your balls, the game loses and you have to restart.
#### 2. Enhancement
You can go to the "witch's hut" enhance your power using magic. A certain amount of magic is needed for each enhancement, but will consume the magic you have. Consider before each purchase!
#### 3. Keyboard Control
Use “WASD” to move the paddle. “W” for moving up, “S” for moving down, “A” for moving left, “D” for moving right.
#### 4. Bricks
![](resources/s_simple.png) Normal brick: no special effects
![](resources/s_strong.png) Thick brick: need two hits to be broken

![](resources/s_solid.png) The Unbreakable: cannot be broken
![](resources/s_ball_block.png) Balls brick: releasing 3 balls once broken

![](resources/s_treasure.png) Bonus brick: gain a large amount of magic power once broken
"""
                Reference _ ->
                     Markdown.toHtml [] """
### References:
1. Cover picture - https://unsplash.com/photos/5DIFvVwe6wk
2. Background music in the game - https://www.bensound.com/royalty-free-music/track/adventure
3. Magical theme background picture - https://unsplash.com/photos/_l4yffWjgt4
4. Background of magic ball - https://www.google.com/imgres?imgurl=http%3A%2F%2Fpic.vjshi.com%2F2019-02-22%2Fc6a8212cd6a00b4d3620682b12744501%2F00003.jpg%3Fx-oss-process%3Dstyle%2Fwatermark&imgrefurl=http%3A%2F%2Fwww.kaimalo.com%2Fimg%2F4c2b8dbc2f7c7aebeabc8a7b7a4c.html&tbnid=nrHZsZfDevirOM&vet=12ahUKEwj1i-X1h43qAhUXhJQKHdbmCh8QMygRegUIARC0AQ..i&docid=KSt2BmBBE9gttM&w=1082&h=1080&q=%E9%AD%94%E6%B3%95%E6%B0%B4%E6%99%B6%E7%90%83&ved=2ahUKEwj1i-X1h43qAhUXhJQKHdbmCh8QMygRegUIARC0AQ
"""
                _ ->
                     Markdown.toHtml [] """
**This should never appear** is reference a game written by pgroup1 of class VG100 in UMJI of SJTU in 2020 summer.
"""
        ]
renderFirstPage : Model -> Html Msg
renderFirstPage model =
    div []
    [
   -- renderBackground 0 0 10000 20000 "resources/Background.jpg"
    renderStartPanel model]


renderMaunalPage : Model -> Html Msg
renderMaunalPage model =

    div [ ]
        [renderInfo model.status
        , div [style "left" "700px"
              ,style "position" "absolute"
              ,style "top" "640px"
              ]
              [generateButton "Home" OpenFirstPage]
        ]

renderGamePage : Model -> Html Msg
renderGamePage model =
    div []
    [ renderGame model
    , renderGamePanel model
    , renderStore model
    , failure model
    , success model
    ]

generateStoreButton : String -> Msg -> Html Msg
generateStoreButton txt msg =
    button
    [ style "height" "60px"
    , style "width" "170px"
    , style "line-height" "60px"
    , style "outline" "none"
    , style "padding" "0"
    , style "border-style" "groove"
    , style "border-color" "black"
    , style "border-width" "2px"
    , style "position" "absolute"
    , style "box-shadow" "-1px -2px -2px 0 moccasin"
    , style "background-color" "grey"
    , style "font-size" "16px"
    , style "border-radius" "6px"
    , style "align" "center"
    , onClick msg]
    [text txt]

renderStore : Model -> Html Msg
renderStore model =
    case model.status of
        Store _ ->
            div
            [ style "background" "rgba(236, 240, 241, 0.89)"
            , style "color" "#34495f"
            , style "height" "400px"
            , style "left" "280px"
            , style "padding" "0 140px"
            , style "position" "absolute"
            , style "top" "155px"
            , style "width" "400px"
            , style "background-image" "url(resources/Store.jpg)"
            , style "background-size" "100% 100%"
            ]
            [ div [style "margin" "20px 0 0 15px", style "color" "white"] [text (shoppingMessage model)]
            , div
                [ style "margin" "70px 0 0 -50px"
                , style "position" "absolute"
                ]
                [generateStoreButton "Longer Paddle:75MP" (BuyBuff LongPaddle)]
            , div
                [ style "margin" "220px 0 0 -50px"
                , style "position" "absolute"
                ]
                [generateStoreButton "Faster Paddle:75MP" (BuyBuff FasterPaddle)]
            , div
                [ style "margin" "70px 0 0 310px"
                , style "position" "absolute"
                ]
                [generateStoreButton "Get 1.5x Magic:150MP" (BuyBuff MoreScore)]
            , div
                [ style "margin" "220px 0 0 310px"
                , style "position" "absolute"
                ]
                [generateStoreButton "One more ball:150MP" (BuyBuff MoreLife)]
            ]
        _ ->
            div [][]

failure : Model -> Html Msg
failure model =
    case model.status of
        Again ->
            div
            [ style "background" "rgba(236, 240, 241, 0.89)"
            , style "color" "#34495f"
            , style "height" "400px"
            , style "left" "280px"
            , style "padding" "0 20px"
            , style "position" "absolute"
            , style "top" "155px"
            , style "width" "600px"
            , style "font-size" "25px"
            ]
            [ text "Very Sorry that all of your Magic Balls have been used up! You have to restart from the beginning"
            , div
                [ style "margin" "250px 0 0 0px"
                , style "position" "absolute"
                , style "left" "250px"
                ]
                [generateButton "Restart" OpenFirstPage]
            ]
        _ ->
            div [][]

success : Model -> Html Msg
success model =
    if model.stage >= 5 then
        div
        [ style "background" "rgba(236, 240, 241, 0.89)"
        , style "color" "#34495f"
        , style "height" "400px"
        , style "left" "280px"
        , style "padding" "0 20px"
        , style "position" "absolute"
        , style "top" "155px"
        , style "width" "600px"
        , style "font-size" "25px"
        ]
        [ text "Congratulations! You have reached the deepest and darkest part of the most fantastic castle! Now the treasure is shining in front of you! You are the one of the best and bravest treasure hunter!"
        , div
            [ style "margin" "250px 0 0 0px"
            , style "position" "absolute"
            , style "left" "250px"
            ]
            [generateButton "Restart" OpenFirstPage]
            ]
        else
            div [][]

shoppingMessage : Model -> String
shoppingMessage model =
    case model.shopping of
        Fine -> "Welcome to my hut. Need some help? Then give your magic power as exchange."
        Poor -> "That requires more than you have..."
        Buy buff -> 
            case buff of
                MoreLife ->
                    "That's a smart choice! You have " ++ String.fromInt model.life ++ " Magic Balls now!"
                LongPaddle ->
                    "That's a smart choice! You have a longer paddle now!"
                FasterPaddle ->
                    "That's a smart choice! Your paddle moves faster now!"
                MoreScore ->
                    "That's a smart choice! You can gain 1.5 time Magic Points each time!"
        Twice -> "Don't be greedy, one is enough for you."

blockListBackground : List Block -> List (Svg Msg)
blockListBackground blocks =
    case blocks of
        [] ->
            []
        head :: rest ->
            let
                headSvg = blockToSvg (backgroundForBlock head) head
                restSvg = blockListBackground rest
            in
            headSvg :: restSvg

backgroundForBlock : Block -> String
backgroundForBlock b =
    case b.category of
        SolidBlock -> "resources/solid.png"
        SimpleBlock -> "resources/simple.png"
        StrongBlock life -> if life == 2 then "resources/strong.png" else "resources/simple.png"
        TreasureBlock _ -> "resources/treasure.png"
        BallBlock -> "resources/ball_block.png"

ballListBackground : List Circle -> String -> List (Svg Msg)
ballListBackground balls image =
    case balls of
            [] ->
                []
            head :: rest ->
                let
                    headSvg = circleToSvg image head
                    restSvg = ballListBackground rest image
                in
                headSvg :: restSvg
