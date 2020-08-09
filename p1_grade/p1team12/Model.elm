module Model exposing (..)

import Debug
import String
import Json.Decode as Decode
import Task
import Time exposing (Posix)
import Random

import Block exposing (..)

--Horrible model
type alias Model =
    { player : Player
    , balls: List Ball
    , paddle: Paddle
    , gameControl: List KeyValue
    , blocks: List Block.Block
    , state : State
    , moveLeft : Bool
    , moveRight : Bool
    , zone : Time.Zone
    , now : Posix
    , seed : Random.Seed
    , prob : Int
    , lastProb : Int
    , difficulty : Difficulty
    , diffClass : DiffClass
    , stage : Int
    , displayState: DisplayState
    , numberOfSkills : Int
    , typeOfSkills : Skill
    , useSkill : Bool
    , fires : List Ball
    }

--Adjust Speed of Bll and Paddles with respect to Difficulty.
type alias DiffClass =
    { ballSpeed : Float
    , paddleSpeed : Float
    }

-- Difficulty Levels.
type Difficulty
    = Easy
    | Medium
    | Difficult

--Control what the GUI will show you
type DisplayState
    = DisplayInit
    | DisplayChoice
    | DisplayTutorial
    | DisplayLevelChoose
    | DisplaySkillChoose
    | DisplayReference
    | DisplayGame
    | DisplayTutorial2

--Three special skills
type Skill
    = FiveBall
    | LongPaddle
    | LongLife

--Game States
type State
    = Pause
    | Play
    | Win
    | Lose
    | WaitStart
    | LoseOneLife
    | WaitNextStage
    | GUI

--The state of current player
type alias Player =
    { lives: Int
    , score: Int
    , addSkill : Int
    }

--Receive signals from the keyboard
type KeyValue
    = Character Char
    | Control String

--Definition of the Ball
type alias Ball =
    { xPos : Float
    , yPos : Float
    , texture : String
    , dx : Float
    , dy : Float
    , exist : Bool
    , ballState : BallState
    }

--Music will be played according to the state of the ball
type BallState
    = PaddleToBlock
    | BlockToPaddle
    | None

--Definition of the paddle
type alias Paddle =
    { xPos : Float
    , yPos : Float
    , texture : String
    , width : Float
    }

--init our Model
init : () -> (Model, Cmd Msg)
init _ =
    let
        model =
            { player = { lives = 3, score = 0, addSkill = 0 }
            , balls = [createBall 37.4 51 0.25 -0.25 True PaddleToBlock, createBall -100 500 0.25 -0.25 False PaddleToBlock, createBall -110 500 0.25 -0.25 False PaddleToBlock, createBall -120 500 0.25 -0.25 False PaddleToBlock, createBall -90 500 0.25 -0.25 False PaddleToBlock, createBall -80 500 0.25 -0.25 False PaddleToBlock, createBall -70 500 0.25 -0.25 False PaddleToBlock ]
            , paddle = initPaddle
            , gameControl = []
            , blocks = Block.initBlock 1
            , state = GUI
            , moveLeft = False
            , moveRight = False
            , zone = Time.utc
            , now = Time.millisToPosix 0
            , seed = Random.initialSeed -1
            , prob = 1
            , lastProb = 0
            , difficulty = Medium
            , diffClass = DiffClass 0.5 1
            , stage = 1
            , displayState = DisplayInit
            , numberOfSkills = 3
            , typeOfSkills = FiveBall
            , useSkill = False
            , fires = [createFire 110 500 0.25 -0.25 False, createFire 120 500 0.25 -0.25 False, createFire 150 500 0.25 -0.25 False, createFire 160 500 0.25 -0.25 False ]
            }
        msg = Task.perform Zone Time.here
    in
        (model, msg)

--Initial our paddle
initPaddle : Paddle
initPaddle =
    Paddle 32 53 "./assets/paddle.jpg" 11

--Create a ball
createBall : Float -> Float -> Float -> Float -> Bool -> BallState -> Ball
createBall x y dx dy exist state=
    Ball x y "./assets/ball.png" dx dy exist state

--Create a fire
createFire : Float -> Float -> Float -> Float -> Bool -> Ball
createFire x y dx dy exist =
    Ball x y "./assets/enemyFire.png" dx dy exist None

--Get the first ball of model.balls
oneBall : Model -> String -> (Ball, Model)
oneBall model kind =
    if kind == "b" then
        case List.head model.balls of
            Just current ->
                (current, {model | balls = tailBalls model kind})
            Nothing ->
                (createBall 37 51 0.5 0.5 True PaddleToBlock, model)
    else
        case List.head model.fires of
            Just current ->
                (current, {model | fires = tailBalls model kind})
            Nothing ->
                (createFire 370 510 0 0 False, model)

--Get the balls of model.balls without the first one
tailBalls : Model -> String -> List Ball
tailBalls model kind=
    if kind == "b" then
        case List.tail model.balls of
            Just current ->
                current
            Nothing ->
                [createBall 37 51 0.5 0.5 True PaddleToBlock]
    else
        case List.tail model.fires of
            Just current ->
                current
            Nothing ->
                [createFire 370 510 0 0 False ]

--Get the second ball of model.balls
twoBall : Model -> String -> (Ball, Model)
twoBall model kind =
    oneBall (Tuple.second (oneBall model kind)) kind

--Get the third ball of model.balls
threeBall : Model -> String -> (Ball, Model)
threeBall model kind =
    oneBall (Tuple.second (twoBall model kind)) kind

--Get the forth ball of model.balls
fourBall : Model -> String -> (Ball, Model)
fourBall model kind =
    oneBall (Tuple.second (threeBall model kind)) kind

--Get the fifth ball of model.balls
fiveBall : Model -> String -> (Ball, Model)
fiveBall model kind =
    oneBall (Tuple.second (fourBall model kind)) kind

--Get the sixth ball of model.balls
sixBall : Model -> String -> (Ball, Model)
sixBall model kind =
    oneBall (Tuple.second (fiveBall model kind)) kind

--Get the seventh ball of model.balls
sevenBall : Model -> String -> (Ball, Model)
sevenBall model kind =
    oneBall (Tuple.second (sixBall model kind)) kind

--Deal with the block that our ball crashes into
dealBlocks : List Block -> Model -> List Block
dealBlocks blocks model=
    if List.isEmpty blocks then
        blocks
    else
        let
            block = currBlock blocks
            x = block.xPos
            y = block.yPos
            nBlocks = List.filter (\block0 -> not ((x == block0.xPos) && (y == block0.yPos))) model.blocks
            deleteBlock =
                case List.length block.textureList of
                    1 ->
                        { block | exist = False }
                    2 ->
                        { block | textureList = List.drop 1 block.textureList }
                    3 ->
                        { block | textureList = List.drop 1 block.textureList }
                    _ ->
                        block
        in
           deleteBlock :: nBlocks

--Judge blockCrash
blockCrash: Model -> Ball -> (String, Model)
blockCrash model ball=
    let
        x = ball.xPos
        y = ball.yPos
        existBlocks = List.filter (\block -> block.exist) model.blocks
        ifCrash = List.filter (\block -> (distance (x,y) ( (minimalPointX x block.xPos 5) ,(minimalPointY y block.yPos 2) )) <= 1) existBlocks
        northCrash = List.filter (\block -> block.yPos + 1 <= y) ifCrash
        southCrash = List.filter (\block -> block.yPos >= y) ifCrash
        westCrash = List.filter (\block -> block.xPos + 1 <= x) ifCrash
        eastCrash = List.filter (\block -> block.xPos >= x) ifCrash
        oldPlayer = model.player
        indicator =
            if (not (List.isEmpty northCrash)) then
                "N"
            else if (not (List.isEmpty southCrash)) then
                "S"
            else if (not (List.isEmpty westCrash)) then
                "W"
            else if (not (List.isEmpty eastCrash)) then
                "E"
            else
                "NO"

        deletedBlocks = List.concat [ westCrash, eastCrash,  northCrash, southCrash ]
        newBlocks = dealBlocks deletedBlocks model
    in
        if List.isEmpty deletedBlocks then
            ("NO", model)
        else
            let
                getBlock = currBlock newBlocks
                updatedPlayer = { oldPlayer | score = (currBlock deletedBlocks).eachScore + oldPlayer.score, addSkill = (currBlock deletedBlocks).eachScore + oldPlayer.addSkill }
                getPaddle = model.paddle
            in
                case getBlock.exist of
                    True ->
                        (indicator, { model | blocks = newBlocks })
                    False ->
                        case getBlock.special of
                            Lifes ->
                                (indicator, { model | blocks = newBlocks, player = { updatedPlayer | lives = updatedPlayer.lives + 1 } })
                            Longer ->
                                if getPaddle.width <= 13 then
                                    (indicator, { model | blocks = newBlocks, player = updatedPlayer, paddle = { getPaddle | width = getPaddle.width + 2 } })
                                else
                                    (indicator, { model | blocks = newBlocks, player = updatedPlayer })
                            Shorter ->
                                if getPaddle.width >= 7 then
                                    (indicator, { model | blocks = newBlocks, player = updatedPlayer, paddle = { getPaddle | width = getPaddle.width - 2 } })
                                else
                                    (indicator, { model | blocks = newBlocks, player = updatedPlayer })
                            Skills ->
                                (indicator, { model | blocks = newBlocks, player = updatedPlayer, numberOfSkills = model.numberOfSkills + 1 })
                            NoSpecial ->
                                (indicator, { model | blocks = newBlocks, player = updatedPlayer })

--The Probability of creating fires
fireWrtDiff : Model -> Int
fireWrtDiff model =
    case model.difficulty of
        Easy ->
            16
        Medium ->
            13
        Difficult ->
            10

--keyDecoder
keyDecoder : Decode.Decoder KeyValue
keyDecoder =
    Decode.map toKeyValue (Decode.field "key" Decode.string)

--Control system
toKeyValue : String -> KeyValue
toKeyValue string =
    let
        _ =
            Debug.log string
    in
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string

--Change the speed of ball and paddle according to the difficulty
judgeDiff : Model -> Model
judgeDiff model =
    case model.difficulty of
        Easy ->
            { model | diffClass = DiffClass 0.35 0.75 }
        Medium ->
            { model | diffClass = DiffClass 0.5 1 }
        Difficult ->
            { model | diffClass = DiffClass 0.7 1.25 }

--Messages
type Msg
    = AddKey KeyValue
    | RemoveKey KeyValue
    | Frame Float
    | Zone Time.Zone
    | Now Posix
    | EasyMsg
    | MediumMsg
    | DifficultMsg
    | NextStage
    | ChangeDisplayState DisplayState
    | ChangeSkillShown Skill

