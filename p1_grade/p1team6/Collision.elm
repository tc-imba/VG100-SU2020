module Collision exposing (Collision(..), reflectBallV, detect)

import Model exposing (Model, Ball, Brick)
import Random

type Collision
    = ToSideWall
    | ToTopWall
    | ToPaddle
    | ToDragon
    | ToBrickSide (List Brick)
    | ToBrickTop (List Brick)
    | ToCorner (List Brick)
    | ToBottom

detect : Model -> Ball -> Maybe Collision
detect model ball = 
    let
        xp = model.paddleX
        x = ball.x
        y = ball.y
        bricks = model.bricks
        scale = model.paddleScale

        topHelper = List.filter (\brick -> x >= Tuple.first brick.pos && x <= Tuple.first brick.pos + 100 
                    && y >= Tuple.second brick.pos - 10 && y <= Tuple.second brick.pos )

        botHelper = List.filter (\brick -> x >= Tuple.first brick.pos && x <= Tuple.first brick.pos + 100 
                    && y >= Tuple.second brick.pos + 20 && y <= Tuple.second brick.pos + 30) 

        sideHelperL = List.filter (\brick -> x >= Tuple.first brick.pos - 10 && x <= Tuple.first brick.pos
                    && y >= Tuple.second brick.pos && y <= Tuple.second brick.pos + 20 )

        sideHelperR = List.filter (\brick -> x >= Tuple.first brick.pos + 100 && x <= Tuple.first brick.pos + 110 
                    && y >= Tuple.second brick.pos && y <= Tuple.second brick.pos + 20 )

        cornerHelperTL = List.filter (\brick -> x <= Tuple.first brick.pos && x>= Tuple.first brick.pos - 10
                    && y >= Tuple.second brick.pos - 10 && y <= Tuple.second brick.pos
                    && (x - Tuple.first brick.pos) ^ 2 + ( y - Tuple.second brick.pos ) ^ 2 <= 100)
        cornerHelperTR = List.filter (\brick -> x <= Tuple.first brick.pos + 110 && x>= Tuple.first brick.pos + 100
                    && y >= Tuple.second brick.pos - 10 && y <= Tuple.second brick.pos
                    && (x - Tuple.first brick.pos - 100) ^ 2 + ( y - Tuple.second brick.pos ) ^ 2 <= 100)
        cornerHelperLL = List.filter (\brick -> x <= Tuple.first brick.pos && x>= Tuple.first brick.pos - 10
                    && y >= Tuple.second brick.pos + 20 && y <= Tuple.second brick.pos + 30
                    && (x - Tuple.first brick.pos) ^ 2 + ( y - Tuple.second brick.pos - 20 ) ^ 2 <= 100)
        cornerHelperLR = List.filter (\brick -> x <= Tuple.first brick.pos + 110 && x>= Tuple.first brick.pos + 100
                    && y >= Tuple.second brick.pos + 20 && y <= Tuple.second brick.pos + 30
                    && (x - Tuple.first brick.pos - 100 ) ^ 2 + ( y - Tuple.second brick.pos - 20 ) ^ 2 <= 100)
        dragonHelper = y < -200 && x ^ 2 + (y + 250) < 3600
    in
    if  x > 390 || x < -390
    then Just ToSideWall

    else if y < -290
    then Just ToTopWall

    else if  
    ( x - xp ) ^ 2 * 30^2 + ( y - 260 ) ^ 2 * 40^2 * scale ^ 2 <= 30^2 * 40^2 * scale ^ 2 && 
    ( x - xp ) ^ 2 * 30^2 + ( y - 270 ) ^ 2 * 40^2 * scale ^ 2 >= 30^2 * 40^2 * scale ^ 2
    then Just ToPaddle

    else if topHelper bricks /= []
        then Just <| ToBrickTop <| topHelper bricks

    else if botHelper bricks /= []
    then Just <| ToBrickTop <| botHelper bricks

    else if sideHelperL bricks /= []
    then Just <| ToBrickSide <| sideHelperL bricks 

    else if sideHelperR bricks /= []
    then Just <| ToBrickSide <| sideHelperR bricks 

    else if cornerHelperTL bricks /= []
    then Just <| ToCorner <| cornerHelperTL bricks

    else if cornerHelperTR bricks /= []
    then Just <| ToCorner <| cornerHelperTR bricks

    else if cornerHelperLL bricks /= []
    then Just <| ToCorner <| cornerHelperLL bricks

    else if cornerHelperLR bricks /= []
    then Just <| ToCorner <| cornerHelperLR bricks

    else if dragonHelper
    then Just ToDragon

    else if y >= 305
    then Just ToBottom

    else Nothing

reflectBallV : Ball -> Model -> Model
reflectBallV ball model = 
    let
        x_ = ball.x + ball.dx
        y_ = ball.y + ball.dy
        dx_ = ball.dx
        dy_ = ball.dy
        newBall = Ball x_ y_ dx_ dy_
        msg = detect model newBall
        dragon = model.dragon
        overSpeed1 v = 
            if v > 5 then 5
            else if v < -5 then -5
            else v
        overSpeed2 v = 
            if v > 5 then 5
            else if v < -5 then -5
            else if v < 0 && v > -3 then -3
            else if v > 0 && v < 3 then 3
            else v
    in
    case msg of 
        Just event ->
            case event of 
                ToTopWall ->
                    { model | balls =  { newBall | dy = -newBall.dy } :: model.balls }

                ToSideWall ->
                    { model | balls =  { newBall | dx = -newBall.dx } :: model.balls }

                ToPaddle ->
                    let
                        ( num1, seed1 ) = Random.step (Random.int -1 1) model.seed
                        ( num2, seed2 ) = Random.step (Random.int -1 1) seed1
                    in
                    
                    if model.active > 0 then
                        { model | balls =  { newBall | dx = overSpeed1 (-newBall.dx + num1 ), dy = -1 * abs (overSpeed2 (-newBall.dy + num2)) } :: model.balls, seed = seed2 }
                    else
                        { model | balls =  newBall :: model.balls  }

                ToBrickTop _ ->
                    { model | balls =  { newBall | dy = -newBall.dy } :: model.balls }

                ToBrickSide _ ->
                    { model | balls =  { newBall | dx = -newBall.dx } :: model.balls }

                ToCorner _ ->
                    { model | balls =  { newBall | dx = -newBall.dx } :: model.balls }

                ToDragon ->
                    { model | balls = { newBall | dx = -newBall.dx, dy = -newBall.dy } :: model.balls, dragon = { dragon | hp = dragon.hp - 1 } }

                ToBottom ->
                    model

        Nothing ->
            { model | balls =  newBall :: model.balls }
