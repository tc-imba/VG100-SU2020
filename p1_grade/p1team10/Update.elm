module Update exposing (..)

import Keyboard
import Keyboard.Arrows
import Model exposing (Ball, Blocks, Blocktype(..), DragState(..), Landmine, Model, Monster, Msg(..), NewBlocks, State(..), gridInit, random)
import Svg.Attributes exposing (xHeight)
import Classic exposing (..)
import Random
import Time
--import Model exposing (grid1)
--import Debugger.Metadata exposing (check)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyMsg -> --get message from keyboard
            ( paddleMove 1 { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none)


        Tock newTime ->
            ( { model | time = newTime }
            |> endCountdown
            , Cmd.none
            )

        Tick time -> --get time to animate the ball
            ( 
                model
                |> changeLimitedTime
                |> ballCollide 
                |> monsterCollide
                |> ballMove time
                |> monsterMove time
                |> monsterDisappear
                |> checkEndGame
            , Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
                , Cmd.none
            )
        Noop ->
            ( model, Cmd.none )

        DragMove fraction ->
            ( paddleMove 2
                { model | dragState =
                        Moving fraction
                }
            , Cmd.none
            )

        PageZero ->
            ({ model | state = Cover }
            , Cmd.none
            )

        PageOne ->
            ({ model | state = Stopped }
            , Cmd.none
            )

        PageTwo ->
            ({ model | state = SecondPage, mode = 1 }
            , Cmd.none
            )
        PageThree ->
            ( { model | state = ThirdPage }
            , Cmd.none
            )

        PageFour ->
            ({ model | state = FourthPage}
            , Cmd.none
            )

        ChooseLevel ->
            let
                grid1 =
                    random (Random.initialSeed 0) 10 (gridInit 10 5 [] 5 1 [(0,0)]) 1 0
            in
            ({model | state = Level
            , grid = grid1.blocks
            , seed = grid1.seed
            , paddle = { px = 250, py = 840, direction = 0 }
            , coX = 250
            , coY = 100
            , ballnum = 1
            , score = 0
            , level = 1
            , monster = []
            , landmine =[]
            , waitime = Nothing
            , waitBlocks = Nothing
            , mode = 2
            , treasureNum = 0
            }
            , Cmd.none
            )

        StartM2L1 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 1
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L2 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 2
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L3 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 3
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L4 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 4
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L5 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 5
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L6 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 6
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L7 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 7
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartM2L8 ->
            (classicChangeGrid {model| pressedKeys = []
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , classiclevel = Just 8
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartModeOne ->
            let
                grid1 =
                    random (Random.initialSeed 0) 10 (gridInit 10 5 [] 5 1 [(0,0)]) 1 0
            in
            ({model | mode = 1
            , pressedKeys = []
            , grid = grid1.blocks
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            --, direction = ( Up, Right )
            , paddle = { px = 250, py = 840, direction = 0 }
            , coX = 250
            , coY = 100
            , ballnum = 1
            , score = 0
            , seed = grid1.seed
            , level = 1
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , monster = []
            , landmine =[]
            , waitime = Nothing
            , waitBlocks = Nothing
            , treasureNum = 0
            , limitedTime = 0
            , limitedTimeLevel = 0
            , state = Countdown
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )

        StartModeThree ->
            let
                grid1 =
                    random (Random.initialSeed 0) 10 (gridInit 10 5 [] 5 1 [(0,0)]) 1 0
            in
            ({model | mode = 3
            , pressedKeys = []
            , grid = grid1.blocks
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            --, direction = ( Up, Right )
            , paddle = { px = 250, py = 840, direction = 0 }
            , coX = 250
            , coY = 100
            , ballnum = 1
            , score = 0
            , seed = grid1.seed
            , level = 1
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , monster = []
            , landmine =[]
            , waitime = Nothing
            , waitBlocks = Nothing
            , treasureNum = 0
            , limitedTimeLevel = 0
            , classiclevel = Nothing
            , limitedTime = Time.posixToMillis model.time + 43000
            , state = Countdown
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )


        Pause ->
             ({ model | state = Paused }, Cmd.none)


        Resume ->
            ( { model | state = Playing}
            , Cmd.none
            )

        Exit ->
            ( { model | state = Stopped}
            , Cmd.none
            )

        StartAgain ->
            let
                grid1 =
                    random (Random.initialSeed 0) 10 (gridInit 10 5 [] 5 1 [(0,0)]) 1 0
            in
            if model.mode == 2 then
            (classicChangeGrid{model |  pressedKeys = []
            , grid = grid1.blocks
            , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
            --, direction = ( Up, Right )
            , paddle = { px = 250, py = 840, direction = 0 }
            , coX = 250
            , coY = 100
            , ballnum = 1
            , score = 0
            , seed = grid1.seed
            , level = 1
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , monster = []
            , landmine =[]
            , waitime = Nothing
            , waitBlocks = Nothing
            , treasureNum = 0
            , limitedTimeLevel = 0
            , limitedTime = Time.posixToMillis model.time + 43000
            , state = Countdown
            , countdownLimit = Time.posixToMillis model.time + 4000
            }
            , Cmd.none
            )
            else
                     ({model |  pressedKeys = []
                    , grid = grid1.blocks
                    , ball = [{ bx = 900, by = 800 , aTK = 1, speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
                    --, direction = ( Up, Right )
                    , paddle = { px = 250, py = 840, direction = 0 }
                    , coX = 250
                    , coY = 100
                    , ballnum = 1
                    , score = 0
                    , seed = grid1.seed
                    , level = 1
                    , zone = Time.utc
                    , time = Time.millisToPosix 0
                    , monster = []
                    , landmine =[]
                    , waitime = Nothing
                    , waitBlocks = Nothing
                    , treasureNum = 0
                    , limitedTimeLevel = 0
                    , limitedTime = Time.posixToMillis model.time + 43000
                    , state = Countdown
                    , countdownLimit = Time.posixToMillis model.time + 4000
                    }
                    , Cmd.none
                    )
        FilterOn ->
            ( {model | filt = 1}
            , Cmd.none
            )

        FilterOff ->
            ( {model | filt = 0}
            , Cmd.none
            )




toFraction : DragState -> Float
toFraction dragState =
  case dragState of
    Static fraction -> fraction
    Moving fraction -> fraction

checkEndGame : Model -> Model
checkEndGame model =
    if List.isEmpty (List.filter (\ ball1 -> ball1.by <= toFloat (model.coY + 800)) model.ball) then
        { model | state = EndPage }
    else
        case model.mode of
            1 ->
                model
            2 ->
                if model.treasureNum >= (generateClassicLevel (Maybe.withDefault 1 model.classiclevel)).treasureNum then
                    { model | state = EndPage }
                else
                    model
            _ ->
                if (model.limitedTime - Time.posixToMillis model.time) // 1000 == 0 && model.score < (model.limitedTimeLevel + 1) * 1000 then
                    { model | state = EndPage }
                else
                    model

endCountdown : Model -> Model
endCountdown model =
    if ((model.countdownLimit - Time.posixToMillis model.time) // 1000) == 0 then
         { model | state = Playing }
    else
         model

paddleMove : Int -> Model -> Model --move paddle according to keyboard
paddleMove num model =
    let
        arrows = Keyboard.Arrows.arrows model.pressedKeys
    in
    case num of
        1 ->
            case arrows.x + 1 of
            2 ->
                if model.paddle.px + 50 > 1460 then
                model
                else
                { model | paddle = { px = model.paddle.px + 50, py = model.paddle.py, direction = 1 } }
            0 ->
                if model.paddle.px - 50 < 240 then
                    model
                else
                { model | paddle = { px = model.paddle.px - 50, py = model.paddle.py, direction = -1 } }
            _ ->
                model
        2 ->
            if 1.5 * toFraction model.dragState < 240 || 1.5 * toFraction model.dragState > 1460 then
                model
            else
                { model | paddle = { direction =
                                        if 1.5 * toFraction model.dragState > model.paddle.px then
                                            1
                                        else if toFraction model.dragState > model.paddle.px then
                                            -1
                                        else
                                            0
                                    , px = 1.5 * toFraction model.dragState, py = model.paddle.py
                                   }
                }
        _ ->
            model

classicChangeGrid : Model -> Model
classicChangeGrid model =
    let
        model1 = {model | level = (generateClassicLevel (Maybe.withDefault 1 model.classiclevel)).startHp}
        grid1 = List.map (\x -> {x | hp = model1.level}) model1.grid
        grid2 = changeGridToClassic grid1
    in
        {model1 | grid = grid2}
       

changeLimitedTime : Model -> Model
changeLimitedTime model =
    if model.limitedTime < Time.posixToMillis model.time then
        {model | limitedTime = Time.posixToMillis model.time + 43000, limitedTimeLevel = model.limitedTimeLevel + 1}
    else
        model

monsterMove : Float -> Model -> Model
monsterMove time model =
    let
        (monsterjustGo, monsterToJudge) = List.partition (\x -> x.statue == Nothing ) model.monster
        (monsterCanGo, monsterToStay) = List.partition (\x -> Maybe.withDefault 1000 x.statue < Time.posixToMillis model.time) monsterToJudge
        changedMonster = List.map (\x -> { x | statue = Nothing}) monsterCanGo
        monstertoGo2 =
            List.map (\ monster -> { monster | bx = monster.bx + Tuple.second monster.direction * 1/6 * time
                                    , by = monster.by + Tuple.first monster.direction * 1/4 * time}) monsterjustGo ++ changedMonster
        monsterList = monstertoGo2 ++ monsterToStay

    in
        {model | monster = monsterList}

ballMove :Float-> Model -> Model --move ball according to its direction value
ballMove time model =
    let
        ballist =
            List.map (\ ball1 -> 
                { bx = ball1.bx + Tuple.second ball1.direction * ball1.speed * time
                , by = ball1.by + Tuple.first ball1.direction * ball1.speed * time
                , aTK = ball1.aTK
                , speed = ball1.speed
                , direction =ball1.direction 
                , target = ball1.target}) model.ball
    in
        {model | ball = ballist}

ballCollide : Model -> Model --check if the ball collides with bricks, box, or paddle and bounce the ball
ballCollide model =
    let
        model1 =
            generateTarget model model.ball
        model2 = 
            effectsImplement model1 (List.map ( \x -> x.target ) model1.ball)
        model3 = landMineBoom model2 (List.filter (\x1 -> x1.time < Time.posixToMillis model2.time) model2.landmine)
    in
    if List.length model3.grid <= 20 then
        generateNewBlocks model3 15
        |> adjustGrid
        |> ballandMonsterCollideBox
        |> ballCollidePaddle
    else
        model3
        |> adjustGrid
        |> ballandMonsterCollideBox
        |> ballCollidePaddle

monsterCollide : Model -> Model
monsterCollide model =
    implementMonsterEffect model model.monster

brickEffects : Model -> Maybe Blocks -> Model
brickEffects model block =
    case block of 
        Just block2 ->
            case block2.types of
                Normal ->
                    { model | score =
                                if block2.hp <= 0 then
                                    model.score + block2.score
                                else
                                    model.score
                            , grid =
                                if block2.hp <= 0 then
                                    List.filter ( \x -> x /= block2 ) model.grid
                                else
                                    model.grid
                    }

                PreTreasure ->
                    { model | score =
                                if block2.hp <= 0 then
                                    model.score + block2.score
                                else
                                    model.score
                            , grid =
                                if block2.hp <= 0 then
                                    List.filter ( \x -> x /= block2 ) model.grid ++ [ { block2 | types = Treasure } ]
                                else
                                    model.grid
                    }

                PreAddBall ->
                    { model | score =
                                if block2.hp <= 0 then
                                    model.score + block2.score
                                else
                                    model.score
                            , grid =
                                if block2.hp <= 0 then
                                    List.filter ( \x -> x /= block2 ) model.grid ++ [ { block2 | types = AddBall } ]
                                else
                                    model.grid
                    }

                PreCrazy ->
                    { model | score =
                                if block2.hp <= 0 then
                                    model.score + block2.score
                                else
                                    model.score
                            , grid =
                                if block2.hp <= 0 then
                                    List.filter ( \x -> x /= block2 ) model.grid ++ [ { block2 | types = Crazy } ]
                                else
                                    model.grid
                    }
                Treasure ->
                    { model | score = model.score + 5 * block2.score, grid = List.filter ( \x -> x /= block2 ) model.grid, treasureNum = model.treasureNum + 1}

                AddBall ->
                    { model | ballnum = model.ballnum + 1
                    , ball = model.ball ++ [{ bx = 900, by = 800 , aTK = max 1 (model.level // 3), speed = 1/2.8, direction = ( -1, 1 ), target = Nothing}]
                    , grid = List.filter ( \x -> x /= block2 ) model.grid
                    }

                LandMine ->
                    { model | landmine = model.landmine ++ [{bx = block2.bx, by = block2.by, time = Time.posixToMillis model.time + 1500}]
                    , grid = List.filter ( \x -> x /= block2 ) model.grid
                    }

                Crazy ->
                    let
                        (ballist, theBall) = List.partition (\x1 -> x1.target /= Just block2) model.ball
                        ajustedBall = List.map (\ x -> {bx = x.bx, by = x.by, aTK = min (x.aTK + 2) 3
                                                , speed = min (1.5*x.speed) 1/1.2
                                                , direction = x.direction, target = x.target}) theBall
                    in
                        {model | ball = ballist ++ ajustedBall, grid = List.filter ( \x -> x /= block2 ) model.grid}

                MonsterIn ->
                    let
                        (x, seed1) = Random.step (Random.float 0.1 1) model.seed
                        (y, seed2) = Random.step (Random.float 0.1 1) seed1
                        (r, seed3) = Random.step (Random.float 25 45) seed2 
                        monster1 = { bx = toFloat block2.bx + 75, by = toFloat block2.by + 37.5, direction = (x,y), statue = Nothing, rmonster = r, seed = seed3, eatenBalls = 0}
                    in
                        { model | monster = 
                                    if block2.hp <= 0 then
                                        model.monster ++ [monster1]
                                    else
                                        model.monster
                                    , grid = 
                                        if block2.hp <= 0 then
                                            List.filter (\x1 ->sqrt (( toFloat x1.bx - monster1.bx )^2 + ( toFloat x1.by - monster1.by )^2) > monster1.rmonster + 110) model.grid 
                                        else
                                            model.grid
                        }
        Nothing ->
            model

landMineBoom : Model -> List Landmine -> Model
landMineBoom model landminelist =
    if List.isEmpty landminelist then
        model
    else
    let
        landmine = Maybe.withDefault {bx = 0, by = 0, time = 0} (List.head landminelist) 
        landminelist1 = List.drop 1 landminelist
        model1 = {model | grid = List.filter (\x ->sqrt (toFloat (( x.bx - landmine.bx )^2 + ( x.by - landmine.by )^2)) > model.bwid) model.grid
                        , ball = List.filter (\x -> sqrt ((( x.bx - toFloat landmine.bx )^2 + ( x.by - toFloat landmine.by )^2)) > model.bwid) model.ball
                        , monster = List.filter (\x -> sqrt ((( x.bx - toFloat landmine.bx )^2 + ( x.by - toFloat landmine.by )^2)) > model.bwid) model.monster
                        , landmine = landminelist1}
    in
        landMineBoom model1 landminelist1



effectsImplement : Model -> List (Maybe Blocks) -> Model
effectsImplement model blocks =
    if List.isEmpty blocks then
        model
    else
    let
        x =
            case List.head blocks of
                Just block1 ->
                    block1
                Nothing ->
                    Nothing
        model1=
            brickEffects model x

        
    in
        effectsImplement model1 (List.drop 1 blocks)


getNewBlocks : Model -> Int -> NewBlocks
getNewBlocks model numberneeded=
        let
            grid1 = Model.gridInit 10 6 model.grid 6 model.level (List.map (\x-> (x.bx, x.by)) model.ball)
            grid2 = Model.random model.seed numberneeded grid1 model.level (List.length model.monster)
            grid6 = List.filter (\x -> x.types /= Normal) grid2.blocks
            grid3 = List.map (\ x -> {bx = x.bx, by = x.by, hp = x.hp
                                    , score = x.score, types = Normal}) grid6
        in
             Model.random grid2.seed 6 grid3 model.level (List.length model.monster)



generateNewBlocks : Model -> Int -> Model
generateNewBlocks model numberneeded = 
    case model.mode of 
        2 ->
            if model.waitime == Nothing then
                let
                    grid4 = getNewBlocks model numberneeded
                    grid5 = changeGridToClassic grid4.blocks
                in
                    {model | seed = grid4.seed, level = model.level + 1, waitBlocks = Just grid5, waitime = Just (Time.posixToMillis model.time + 3000)}
            else
                implementNewBlocks model
        _ ->
            if model.waitime == Nothing then
                let
                    grid4 = getNewBlocks model numberneeded
                in
                    { model | seed = grid4.seed, level = model.level + 1, waitBlocks = Just grid4.blocks, waitime = Just (Time.posixToMillis model.time + 3000)}
            else
                implementNewBlocks model


implementNewBlocks : Model -> Model
implementNewBlocks model =
    case model.waitime of 
        Nothing ->
            model
        Just waittime ->
            if Time.posixToMillis model.time > waittime then
                let
                    model1 = {model | grid = model.grid ++ Maybe.withDefault [] model.waitBlocks, waitime = Nothing}
                in
                    {model1 | waitBlocks = Nothing}
            else
                model


type alias CheckList =
    { brick : Blocks
    , check : Bool
    , directionFirst : Float
    , directionSecond : Float
    , check5678 : Bool
    , addy : Int
    , addx : Int
    }

type alias DeterminedTarget =
    { first : CheckList
    , second : CheckList
    , third : CheckList
    , fourth : CheckList
    }

possibleBricks : Float -> Float -> Float->  Model -> (Float, Float) -> Float -> DeterminedTarget
possibleBricks bx by rball model direction speed=
    let
        ro = floor ((by - toFloat model.coY) / 75)
        co = floor ((bx - toFloat model.coX) / 150)
        brick1 = { bx = model.coX + co * 150, by = model.coY + (ro - 1) * 75, hp = 1,score = 30, types = Normal } --Up               get the four positions of potential bricks around the current ball
        brick2 = { bx = model.coX + (co - 1) * 150, by = model.coY + ro * 75, hp = 1,score = 30, types = Normal } --Left
        brick3 = { bx = model.coX + (co + 1) * 150, by = model.coY + ro * 75, hp = 1,score = 30, types = Normal } --Right
        brick4 = { bx = model.coX + co * 150, by = model.coY + (ro + 1) * 75, hp = 1,score = 30, types = Normal } --Down
        brick5 = { bx = model.coX + (co - 1) * 150, by = model.coY + (ro + 1) * 75, hp = 1,score = 30, types = Normal } --LeftDown
        brick6 = { bx = model.coX + (co + 1) * 150, by = model.coY + (ro + 1) * 75, hp = 1,score = 30, types = Normal } --RightDown
        brick7 = { bx = model.coX + (co + 1) * 150, by = model.coY + (ro - 1) * 75, hp = 1,score = 30, types = Normal } --RightUp
        brick8 = { bx = model.coX + (co - 1) * 150, by = model.coY + (ro - 1) * 75, hp = 1,score = 30, types = Normal } --LeftUp
        check1 : Bool
        check1 = by - rball < toFloat (brick1.by + 85) && Tuple.first direction < 0 --&& ball.bx > toFloat (brick8.bx +145) && ball.bx < toFloat (brick7.bx - 5)--check if the ball collides with the four potential bricks
        check2 : Bool
        check2 = bx - rball < toFloat (brick2.bx + 160) && Tuple.second direction < 0--&& ball.by > toFloat (brick8.by +70 ) && ball.by < toFloat (brick5.by + 5)
        check3 : Bool
        check3 = bx + rball > toFloat brick3.bx - 10 && Tuple.second direction > 0--&& ball.by > toFloat (brick7.by +70) && ball.by < toFloat (brick6.by + 5)
        check4 : Bool
        check4 = by + rball > toFloat brick4.by - 10 && Tuple.first direction > 0--&& ball.bx > toFloat (brick7.bx +145) && ball.bx < toFloat (brick7.bx - 5)
        check5 : Bool
        check5 = sqrt ((bx - toFloat (brick5.bx + 150)) ^ 2 + (by - toFloat brick5.by) ^ 2) < rball + 10
        check6 : Bool
        check6 = sqrt ((bx - toFloat brick6.bx) ^ 2 + (by - toFloat brick6.by) ^ 2) < rball + 10
        check7 : Bool
        check7 = sqrt ((bx - toFloat brick7.bx) ^ 2 + (by - toFloat (brick7.by + 75)) ^2 ) < rball + 10
        check8 : Bool
        check8 = sqrt ((bx - toFloat (brick8.bx + 150) ) ^ 2 + (by - toFloat (brick8.by + 75)) ^ 2) < rball +10

        blockslist = [{brick = brick1, check = check1, directionFirst = -1, directionSecond = 1, check5678 = False, addy = 0, addx = 0}
                    ,{brick = brick2, check = check2, directionFirst = 1, directionSecond = -1, check5678 = False, addy = 0, addx = 0}
                    ,{brick = brick3, check = check3, directionFirst = 1, directionSecond = -1, check5678 = False, addy = 0, addx = 0}
                    ,{brick = brick4, check = check4, directionFirst = -1, directionSecond = 1, check5678 = False, addy = 0, addx = 0}
                    ,{brick = brick5, check = check5, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 0, addx = 150}
                    ,{brick = brick6, check = check6, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 0, addx = 0}
                    ,{brick = brick7, check = check7, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0}
                    ,{brick = brick8, check = check8, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 150}]

        list1 = List.filter (\x -> List.member (x.brick.bx, x.brick.by) (List.map (\x1 -> (x1.bx, x1.by)) model.grid) && x.check && x.check5678 == False) blockslist

        head1 = Maybe.withDefault {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = False, addy = 0, addx =0} (List.head list1)

        list2 = List.filter (\x -> List.member (x.brick.bx, x.brick.by) (List.map (\x1 -> (x1.bx, x1.by)) model.grid) && x.check && x.check5678 == True) blockslist

        (list3, list4) = List.partition (\x -> (k * toFloat x.brick.bx + b - toFloat (x.brick.by + x.addy)) * (k * toFloat (x.brick.bx +150) + b - toFloat (x.brick.by + x.addy)) < 0) list2

        (list5, list6) = List.partition (\x -> (k * toFloat (x.brick.bx + x.addx) + b - toFloat x.brick.by) * (k * toFloat (x.brick.bx + x.addx) + b - toFloat (x.brick.by + 75)) < 0) list4

        head2 = Maybe.withDefault {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 0, addx = 150} (List.head list3)

        head3 = Maybe.withDefault {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0} (List.head list5)

        head4 = Maybe.withDefault {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0} (List.head list6)

        k = Tuple.first direction * speed / (Tuple.second direction * speed)

        b= by - k * bx
    in
        {first = head1, second = head2, third = head3, fourth = head4}



checkBrick : Ball -> Model-> Model --check if and in which direction would the brick collide with the ball, and return the direction and position of the brick
checkBrick ball model =
    let
        determinedTarget = possibleBricks ball.bx ball.by model.rball model ball.direction ball.speed

    in
    if determinedTarget.first /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = False, addy = 0, addx = 0}  then

        changeBallGrid ball determinedTarget.first.brick determinedTarget.first.directionFirst determinedTarget.first.directionSecond model

    else if determinedTarget.second /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 0, addx = 150} then

        changeBallGrid ball determinedTarget.second.brick determinedTarget.second.directionFirst determinedTarget.second.directionSecond model

    else if determinedTarget.third /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0} then

        changeBallGrid ball determinedTarget.third.brick -determinedTarget.second.directionFirst -determinedTarget.second.directionSecond model

    else if determinedTarget.fourth /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0} then

        model
    else

        model
        
monsterCheckBrick : Monster -> Model -> Model
monsterCheckBrick monster model =
    let
        determinedBlock = possibleBricks monster.bx monster.by monster.rmonster model monster.direction (1/4.7)
    in 
    if determinedBlock.first /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = False, addy = 0, addx =0}  then

        blocksChangeMonsterDirection monster determinedBlock.first.directionFirst determinedBlock.first.directionSecond model
    
    else if determinedBlock.second /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 0, addx =150} then

        blocksChangeMonsterDirection monster determinedBlock.second.directionFirst determinedBlock.second.directionSecond model

    else if determinedBlock.third /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx =0} then

        blocksChangeMonsterDirection monster -determinedBlock.second.directionFirst -determinedBlock.second.directionSecond model

    else if determinedBlock.fourth /= {brick = {bx = 100, by = 600, hp = 3, score = 30, types = Normal }
                                   , check = False, directionFirst = -1, directionSecond = 1, check5678 = True, addy = 75, addx = 0} then

        model

    else

        model

changeBallGrid : Ball -> Blocks ->  Float -> Float  -> Model -> Model
changeBallGrid ball brick directionFirst directionSecond model =
    let
            corrsblock = Maybe.withDefault {bx = 100, by = 600, hp = 3, score = 30, types = Normal } (List.head (List.filter (\x -> (x.bx, x.by) == (brick.bx, brick.by)) model.grid))
            targettedBlock = {corrsblock | hp = corrsblock.hp - ball.aTK}
            grid1 = List.filter (\x -> (x.bx, x.by) /= (brick.bx, brick.by)) model.grid ++ [targettedBlock]
            ball1 = {ball | bx = ball.bx + ((directionSecond - 1 )*5/2)*Tuple.second ball.direction, by = ball.by + ((directionFirst - 1 )*5/2)*Tuple.first ball.direction
                                                                    , target = Just targettedBlock}
            ball2 = {ball1 | direction = (directionFirst * Tuple.first ball.direction , directionSecond * Tuple.second ball.direction)}
            ballist = List.filter (\x -> x /= ball) model.ball ++ [ball2] --(directionFirst * Tuple.first ball.direction , directionSecond * Tuple.second ball.direction)

        in
        { model | grid = grid1, ball = ballist}

blocksChangeMonsterDirection : Monster -> Float -> Float -> Model -> Model
blocksChangeMonsterDirection monster directionFirst directionSecond model =
    let
        monster1 = {monster | bx = monster.bx + ((directionSecond - 1 )*10/2)*Tuple.second monster.direction, by = monster.by + ((directionFirst - 1 )*10/2)*Tuple.first monster.direction}
        monster2 = {monster1 | direction = (directionFirst * Tuple.first monster.direction , directionSecond * Tuple.second monster.direction)}
        monsterlist = List.filter (\x -> x /= monster) model.monster ++ [monster2]
    in
        {model | monster = monsterlist}


generateTarget : Model -> List Ball -> Model     -- List.filter (\ x -> (Maybe.withDefault {bx = 0, by = 0, hp = 1, score = 30, types = Normal } x).hp <= 0) targets
generateTarget model ballist=
    if List.isEmpty ballist then
        model
    else
    let 
        a = Maybe.withDefault {bx = 0, by = 0, aTK = 1, speed = 1/3.3, direction = ( -1, 1 ), target = Nothing} (List.head ballist)

        model1 = checkBrick a model
    in
        generateTarget model1 (List.drop 1 ballist)

killBalls : Monster -> Model -> Model
killBalls monster model =
    if not (List.isEmpty (List.filter (\x -> sqrt ((x.bx - monster.bx)^2 + (x.by - monster.by)^2) < monster.rmonster + model.rball) model.ball)) then
        if monster.statue == Nothing then
            let
                ballist = List.filter (\x -> sqrt ((x.bx - monster.bx)^2 + (x.by - monster.by)^2) > monster.rmonster + model.rball) model.ball
                timetoMove = Time.posixToMillis model.time + 4000
                monster1 = {monster | statue = Just timetoMove, eatenBalls = monster.eatenBalls + 1}
                monsterlist = List.filter (\x -> abs (x.bx - monster.bx) > 1 && abs (x.by - monster.by) > 1) model.monster ++ [monster1]
            in
                {model | ball = ballist, monster = monsterlist}
        else
            model
    else
        model

implementMonsterEffect : Model -> List Monster -> Model
implementMonsterEffect model monsterlist =
    if List.isEmpty monsterlist then
        model
    else
    let 
        a = Maybe.withDefault {bx = 0, by = 0, direction = (-1, 1), statue = Nothing, rmonster = 60, seed = model.seed, eatenBalls = 0} (List.head monsterlist)

        model1 = monsterCheckBrick a model

        model2 = killBalls a model1

    in
        implementMonsterEffect model2 (List.drop 1 monsterlist)

monsterDisappear : Model -> Model
monsterDisappear model =
    let
        monsterList = List.filter (\x -> x.eatenBalls < 3) model.monster
    in
        {model | monster = monsterList}



adjustGrid : Model -> Model
adjustGrid model =
    { model |  ball = List.map (\x1 -> {bx = x1.bx, by = x1.by, aTK = x1.aTK, speed = x1.speed, direction =x1.direction ,target = Nothing}) model.ball }


checkWall : Ball -> Model -> Ball --check if and in which direction of the wall would collide with the ball
checkWall ball model=
    let
        check1 : Bool
        check1 = (ball.by - model.rball) < toFloat model.coY && Tuple.first ball.direction < 0
        check2 : Bool
        check2 = (ball.bx - model.rball) < toFloat model.coX  && Tuple.second ball.direction < 0
        check3 : Bool
        check3 = (ball.bx + model.rball) > toFloat (model.coX + 1498) && Tuple.second ball.direction > 0
    in
    if check1 then
        {ball | direction =  ( -(Tuple.first ball.direction) , Tuple.second ball.direction ), by = toFloat model.coY + model.rball +5}
    else if check2 then
        {ball | direction = ( Tuple.first ball.direction , -(Tuple.second ball.direction ) ), bx = toFloat model.coX + model.rball +5}
    else if check3 then
        {ball | direction = ( Tuple.first ball.direction , -(Tuple.second ball.direction ) ), bx = toFloat model.coX - model.rball +1493}
    else
        ball

monsterCheckWall : Monster -> Model -> Monster
monsterCheckWall monster model =
    let
        check1 : Bool
        check1 = (monster.by - monster.rmonster) < toFloat model.coY 
        check2 : Bool
        check2 = (monster.bx - monster.rmonster) < toFloat model.coX 
        check3 : Bool
        check3 = (monster.bx + monster.rmonster) > toFloat (model.coX + 1498)
        check4 : Bool
        check4 = (monster.by + monster.rmonster) > toFloat model.coY + 650

        (x1, seed1) = Random.step (Random.float 4 10) monster.seed

        (x2, seed2) = Random.step (Random.float 4 10) seed1
    in
    if check1 then
        {monster | direction =  ( -(Tuple.first monster.direction)/abs(Tuple.first monster.direction) * x1/sqrt( (x1^2)*2 + (x2^2)*2 ) 
                                , Tuple.second monster.direction/abs(Tuple.second monster.direction) * x2/sqrt( (x1^2)*2 + (x2^2)*2 ) )
                                , by = toFloat model.coY + monster.rmonster +10, seed = seed2}
    else if check2 then
        {monster | direction = ( Tuple.first monster.direction/abs(Tuple.first monster.direction) * x1/sqrt( (x1^2)*2 + (x2^2)*2 ) 
                                , -(Tuple.second monster.direction )/abs(Tuple.second monster.direction) * x2/sqrt( (x1^2)*2 + (x2^2)*2 ) )
                                , bx = toFloat model.coX + monster.rmonster +10, seed = seed2}
    else if check3 then
        {monster | direction = ( Tuple.first monster.direction/abs(Tuple.first monster.direction) * x1/sqrt( (x1^2)*2 + (x2^2)*2 ) 
                                , -(Tuple.second monster.direction )/abs(Tuple.second monster.direction) * x2/sqrt( (x1^2)*2 + (x2^2)*2 ) )
                                , bx = toFloat model.coX - monster.rmonster +1490, seed = seed2}
    else if check4 then
        {monster | direction =  ( -(Tuple.first monster.direction)/abs(Tuple.first monster.direction) * x1/sqrt( (x1^2)*2 + (x2^2)*2 ) 
                                , Tuple.second monster.direction/abs(Tuple.second monster.direction) * x2/sqrt( (x1^2)*2 + (x2^2)*2 ) )
                                , by = toFloat model.coY - monster.rmonster +640, seed = seed2}
    else
        monster


ballandMonsterCollideBox : Model -> Model 
ballandMonsterCollideBox model =
    let
        ballist = List.map (\ x -> checkWall x model) model.ball
        monsterlist = List.map (\ x -> monsterCheckWall x model) model.monster
    in
        {model | ball = ballist, monster = monsterlist}



checkPaddle : Ball -> Model -> Ball --check if the paddle would collide with the ball
checkPaddle ball model =
    if (ball.bx + 8) > (model.paddle.px - 10) && (ball.bx - 8) < (model.paddle.px + model.pwid +10) && (ball.by + model.rball) < (model.paddle.py + model.phei +10) then
        if Tuple.second ball.direction > 0 then
            if (ball.by + model.rball) > (model.paddle.py - 5) && Tuple.first ball.direction > 0 then
                { ball | direction =
                             if model.paddle.direction > 0 then
                                 ( -1 * sqrt (2 - (min 1.3 ((Tuple.second ball.direction) * 1.3))), min 1.3 ((Tuple.second ball.direction) * 1.3))
                             else if model.paddle.direction < 0 then
                                 ( -1 * sqrt (2 - (max 0.5 ((Tuple.second ball.direction) / 1.3))), max 0.5 ((Tuple.second ball.direction) / 1.3))
                             else
                                 ( -1 * Tuple.first ball.direction, Tuple.second ball.direction )
                }
            else
                ball
        else
            if (ball.by + model.rball) > (model.paddle.py - 5) && Tuple.first ball.direction > 0 then
                { ball | direction =
                             if model.paddle.direction > 0 then
                                 ( -1 * sqrt (2 + min (-1 * 0.5) ((Tuple.second ball.direction) / 1.3)), min (-1 * 0.5) ((Tuple.second ball.direction) / 1.3))
                             else if model.paddle.direction < 0 then
                                 ( -1 * sqrt (2 + max (-1 * 1.3) ((Tuple.second ball.direction) * 1.3)), max (-1 * 1.3) ((Tuple.second ball.direction) * 1.3))
                             else
                                 ( -1 * Tuple.first ball.direction, Tuple.second ball.direction )
                }
            else
                ball
    else
        ball

ballCollidePaddle : Model -> Model
ballCollidePaddle model =
    let
        ballist = List.map (\ x -> checkPaddle x model) model.ball
    in
        {model | ball = ballist}
