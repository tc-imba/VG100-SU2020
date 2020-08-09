module View exposing (view)

import Model exposing (..)
import Update exposing (Msg(..), Choice(..))

import Html exposing (Html, div, text, h1, button)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, viewBox, fill, fillOpacity, x, y, stroke, cx, cy, rx, ry, r, xlinkHref, opacity, transform)
import Html.Attributes exposing (title)
import String exposing (fromInt)
import Json.Decode exposing (index)
-- 这里因为冲突keyword而exposing，太丑了，我再想想办法QAQ

renderImage : Int -> Int -> String -> Int -> Int -> Int -> Float -> Svg msg
renderImage w h src x y scale angle =
    Svg.image
        (  xlinkHref src
        :: width (fromInt w)
        :: height (fromInt h)
        :: transform ("translate(" ++ fromInt ((-w )//2 + x) ++ "," ++ fromInt (-h//2 + y) ++ ") scale(" ++ fromInt scale ++ ",1)" ++ " rotate(" ++ String.fromFloat angle ++ ")")
        :: []
        )
        []

-- -- 砖块
-- renderBrick : Brick -> Svg Msg
-- renderBrick brick = 
--     rect
--         [ x (fromInt <| Tuple.first brick.pos)
--         , y (fromInt <| Tuple.second brick.pos)
--         , width "100"
--         , height "20"
--         , rx "2"
--         , ry "2"
--         , fill "#09AAAA"
--         , fillOpacity (brickColor brick)
--         ]
--         []
renderBrick : Brick -> Svg Msg
renderBrick brick = 
    let
        path = "./assets/brick" ++ (if brick.hp < 2 then "/awake" else "/asleep") ++ ".png"
    in
    
    renderImage 100 20 path (Tuple.first brick.pos + 50) (Tuple.second brick.pos + 10) 1 0


renderBricks : List Brick -> List (Svg Msg)
renderBricks bricks = 
    List.map renderBrick bricks

-- 球
renderBall : Ball -> Svg Msg
renderBall ball = 
    circle
        [ cx (fromInt ball.x)
        , cy (fromInt ball.y)
        , r "10"
        , stroke "#888888"
        ]
        []

-- 待定：球要图吗
renderBallGif : Model -> Ball -> Svg Msg
renderBallGif model ball = 
    let
        root = "./assets/ball/" 
        record = if ball.dx == 0 && ball.dy == 0 then 1 else modBy 9 (model.imageRecord//5) + 1
        src = ( root ++ fromInt record ++ ".png" )
        w = 70
        h = 70
        x = ball.x
        y = ball.y
        angleZero = 225
        angleDelta = Tuple.second <| toPolar (toFloat ball.dx, toFloat ball.dy)
        angle = angleZero + angleDelta / pi * 180

    in
    Svg.image
        (  xlinkHref src
        :: width (fromInt w)
        :: height (fromInt h)
        :: transform ("translate(" ++ fromInt ((-w )//2 + x) ++ "," ++ fromInt (-h//2 + y) ++ ") rotate(" ++ String.fromFloat angle ++ ", 35, 35)")
        :: []
        )
        []

renderBallGifs : Model -> List (Svg Msg)
renderBallGifs model =
    List.map (renderBallGif model) model.balls

renderBalls : List Ball -> List (Svg Msg)
renderBalls balls = 
    List.map renderBall balls

-- 骑士
renderKnight : Model -> List (Svg msg)
renderKnight model = 
    let
        scale = model.paddleScale
        path = if model.active > 0 then ["/attack", "/trace"] else ["/run"]
        dir = if model.paddleV  > 0 then "/right"
              else if model.paddleV  < 0 then "/left"
              else 
                  if model.paddleVBefore > 0
                  then 
                      "/right"
                  else 
                      "/left"
        record = modBy 4 (model.imageRecord//5) + 1
        frame = if model.active > 0 then ""
                else if model.paddleV == 0 then ""
                else "/" ++ fromInt record
        first = renderImage 80 80 ("./assets/knight" ++ Maybe.withDefault "" (List.head path) ++ dir ++ frame ++ ".gif" ) model.paddleX 260 1 0
        second = if model.paddleV  > 0 then renderImage 80 80 ("./assets/knight" ++ Maybe.withDefault "" (List.head (List.drop 1 path)) ++ dir ++ frame ++ ".gif" )
         (model.paddleX-45*scale + 45) 260 scale 0
              else if model.paddleV  < 0 then renderImage 80 80 ("./assets/knight" ++ Maybe.withDefault "" (List.head (List.drop 1 path)) ++ dir ++ frame ++ ".gif" )
         (model.paddleX-35*scale + 35) 260 scale 0
              else 
                  if model.paddleVBefore > 0
                  then 
                      renderImage 80 80 ("./assets/knight" ++ Maybe.withDefault "" (List.head (List.drop 1 path)) ++ dir ++ frame ++ ".gif" )
         (model.paddleX-45*scale + 45) 260 scale 0
                  else 
                      renderImage 80 80 ("./assets/knight" ++ Maybe.withDefault "" (List.head (List.drop 1 path)) ++ dir ++ frame ++ ".gif" )
         (model.paddleX-35*scale + 35) 260 scale 0
        list = if model.active > 0 then [ first , second ]
               else [ first ]
    in
        list

-- 龙
renderDragon : Model -> Svg msg
renderDragon model =
    let
        dragon = if model.stage == 1 then "/asleep" else "/awake"
        path = "./assets/dragon" ++ dragon ++ ".gif"
    in
    renderImage 200 200 path 0 -265 1 0

renderAttack : Model -> List (Svg msg)
renderAttack model =
    let
        ballList = model.dragon.attack
        tanBall ball = 180 + 180 * (atan2 (toFloat ball.dy) (toFloat ball.dx)) / pi 
    in
        List.map (\ball -> if ball.dx > 8
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 200 ) ( ball.y ) 1 (tanBall ball + 180)
                           else if ball.dx == 1
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 180 ) ( ball.y - 20 ) 1 (tanBall ball + 180)
                           else if ball.dx == 2
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 160 ) ( ball.y - 40 ) 1 (tanBall ball + 180)
                           else if ball.dx == 3
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 135 ) ( ball.y - 47 ) 1 (tanBall ball + 180)
                           else if ball.dx == 4
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 113 ) ( ball.y - 50 ) 1 (tanBall ball + 180)
                           else if ball.dx == 5
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 95 ) ( ball.y - 53 ) 1 (tanBall ball + 180)
                           else if ball.dx == 6
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 82 ) ( ball.y - 50 ) 1 (tanBall ball + 180)
                           else if ball.dx == 7
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 70 ) ( ball.y - 50 ) 1 (tanBall ball + 180)
                           else if ball.dx == 8
                           then renderImage 200 200 "./assets/shadow_right.png" 
                                ( ball.x + 60) ( ball.y - 48 ) 1 (tanBall ball + 180)
                           else if ball.dx == -1 
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 23 ) ( ball.y + 35 * ball.dy ) 1 (tanBall ball)   
                           else if ball.dx == -2 
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 34 ) ( ball.y + 30 * ball.dy ) 1 (tanBall ball)   
                           else if ball.dx == -3
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 33 ) ( ball.y + 25 * ball.dy ) 1 (tanBall ball)  
                           else if ball.dx == -4
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 36 ) ( ball.y + 22 * ball.dy ) 1 (tanBall ball) 
                           else if ball.dx == -5
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 35 ) ( ball.y + 19 * ball.dy ) 1 (tanBall ball) 
                           else if ball.dx == -6
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 36 ) ( ball.y + 17 * ball.dy ) 1 (tanBall ball) 
                           else if ball.dx == -7
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 35 ) ( ball.y + 15 * ball.dy ) 1 (tanBall ball)
                           else if ball.dx == -8
                           then
                                renderImage 200 200 "./assets/shadow_left.png" 
                                ( ball.x - 32 ) ( ball.y + 13 * ball.dy ) 1 (tanBall ball)
                           else
                                if ball.x > 0
                                then
                                    renderImage 200 200 "./assets/shadow_left.png" 
                                    ( ball.x - 30 * ball.dx ) ( ball.y + 40 * ball.dy ) 1 (tanBall ball)
                                else 
                                    renderImage 200 200 "./assets/shadow_right.png" 
                                    ( ball.x + 200 ) ( ball.y ) 1 (tanBall ball + 180)
                 ) ballList

renderHp : Model -> List ( Svg msg )
renderHp model =
    let 
        hp = model.dragon.hp
        remain = rect   [ x "-180"
                        , y "-299"
                        , width ( fromInt ( 120 * hp ) )
                        , height "10"
                        , fill "#FF0000"][]
        lost = rect     [ x ( fromInt ( 120 * hp  - 180 ) )
                        , y "-299"
                        , width ( fromInt ( 360 - 120 * hp ) )
                        , height "9"
                        , fill "#A8A8A8"][]
    in 
        [ remain,lost ]

-- 游戏
renderGame : Model -> Html Msg
renderGame model = 
    div 
    [ style "position" "absolute"
    , style "left" "0"
    , style "top" "0"
    , style "bottom" "0"
    ]
    [ div
        [ style "background-color" "rgba(150,150,150,0.4)"

        ]
        [ svg
            [ viewBox "-400 -300 800 600"
            , width "800"
            , height "600"
            , style "border" "1px solid #666"
            ]
            ( 
            -- renderBalls model.balls
            renderBallGifs model
            ++ renderBricks model.bricks
            ++ renderKnight model
            ++ renderAttack model
            ++ renderHp model
            ++ [ renderDragon model ] 
            )
        ]
    ]

-- 结局场景
renderEnd : Model -> Html Msg
renderEnd model = 
    let 
        end = case model.status of
            END 0 -> "gameOver"
            END 1 -> "wakeUp"
            END 2 -> "reincarnation"
            END 3 -> "friendship"
            _ -> ""
        path = "./assets/ending/" ++ end ++ ".png" 
    in
        div 
        [ style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "bottom" "0"
        , style "outline" "1px solid black"
        ]
        [ div
            []
            [ svg
                [ viewBox "-400 -300 800 600"
                , width "800"
                , height "600"
                ]
                [renderImage 800 600 path 0 0 1 0]
            ]
        ]

-- 导航栏
-- renderTitle : String -> Html Msg
-- renderTitle txt =
--     div
--         -- [ style "color" "#34495f"
--         -- , style "font-size" "40px"
--         -- , style "line-height" "60px"
--         -- , style "margin" "30px 0 0"
--         -- ]
--         [ class "title"
--         ]
--         [ text txt ]

renderItem : String -> Html Msg
renderItem txt =
    div
        [ style "color" "#bdc3c7"
        , style "font-weight" "300"
        , style "line-height" "1"
        , style "margin" "30px 0 0"
        ]
        [ text txt ]

renderNum : Int -> Html Msg
renderNum n =
    div
        [ style "color" "#3993d0"
        , style "font-size" "30px"
        , style "line-height" "1"
        , style "margin" "5px 0 0"
        ]
        [ text (fromInt n) ]


-- 几个交互按钮
renderButton : Model -> Html Msg
renderButton model = 
    let
        (str, msg) = 
            case model.status of
                Playing ->
                    ("||", Pause)
                
                Paused ->
                    ("|>", Resume)
                
                _ ->
                    (">_<", Noop)
    in
    button
        [ class "button_pause" 
        , onClick msg
        ]
        [ text str ]

renderYChoiceButton : Model -> Html Msg
renderYChoiceButton model = 
    let
        (str, msg) = 
            case model.status of
                Win stage ->
                    if stage == 1 then
                        ("Y", Choose Chase1)
                    else if stage == 2 then
                        ("Y", Choose Chase2)
                    else ("", Noop)
                
                Lose _ ->
                    ("Y", Retry) 
                
                _ ->
                    ("", Noop)
    in
    button
        [ class "button_choice"
        , style "left" "110px"
        , style "background-position-x" "center"
        , onClick msg
        ]
        [ text str ]

renderNChoiceButton : Model -> Html Msg
renderNChoiceButton model = 
    let
        (str, msg) = 
            case model.status of
                Win stage ->
                    if stage == 1 then
                        ("N", Choose Quit1)
                    else if stage == 2 then
                        ("N", Choose Quit2)
                    else ("", Noop)
                
                Lose stage ->
                    if stage == 1 then
                        ("N", Choose Quit1)
                    else if stage == 2 then
                        ("N", Choose Quit2)
                    else ("", Noop)
                _ ->
                    ("", Noop)
    in
    button
        [ class "button_choice"
        , style "left" "180px"
        , style "background-position-x" "right"
        , onClick msg
        ]
        [ text str ]

renderSideBar : Model -> Html Msg
renderSideBar model = 
    div
        [ style "width" "200px"
        , style "height" "600px"
        , style "font-family" "Fira Code"
        , style "font-size" "14px"
        , style "border" "1px solid #666" 
        , style "padding" "0 30px"
        , style "position" "absolute"
        , style "left" "800px"
        , style "right" "0"
        , style "top" "0"
        , style "background-color" "rgba(79,79,79,0.3)"
        ]
        [ renderItem "Power"
        , renderNum model.score
        , renderItem "Stage"
        , renderNum model.stage
        , renderButton model
        , renderItem "Ray's Level"
        , renderNum (model.score //300 + 1)
        , renderYChoiceButton model
        , renderNChoiceButton model
        ]

-- View主函数
view : Model -> Html Msg
view model =
    let
        content = 
            case model.status of
                END x ->
                    [ renderEnd model
                    , renderSideBar model
                    ]

                _ ->
                    [ renderGame model
                    , renderSideBar model
                    ]
    in

    --  div 
    --     [ style "width" "60%"
    --     , style "position" "absolute"
    --     , style "left" "20%"
    --     , style "right" "20%"
    --     ]
    --     content
    div
        [ class "elm_app" ]
        content
