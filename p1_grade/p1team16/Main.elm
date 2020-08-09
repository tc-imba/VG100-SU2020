module Main exposing (..)

--elm install elm/svg
--elm install elm/random

import Browser
import Browser.Events
import Html.Events exposing (onClick)
import Html exposing (Html, button, div, img)
import Html.Attributes exposing (src)
import Json.Decode as Decode
import Process
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Random


type Msg
    = Animation Float
    | ButtonPressed PlayerCommand --when button on keyboard was pressed, attach the command to this message
    | NoButtonPressed PlayerCommand --when no button on keyboard was pressed, attach the command to this message (to not move the paddle etc.)
    | Respawn
    | ChooseStage Stages
    | ChooseBonus Int
    | GenerateRandomNumber
    | NewRandomNumber Int
    | NewRandomBonus Int
    | Backup --to allow nothing to happen after the button is pressed



type PlayerCommand
    = MovePaddleLeft --command to move the paddle to the left
    | MovePaddleRight
    | LaserFire
    | Change GameStatus Stages
    | Reset GameStatus


type PaddleMove
    = MoveLeft --paddle movement/status
    | MoveRight
    | NotMoving


type GameStatus
    = Playing StatusOfPlaying
    | Died
    | Win
    | Lose
    | Start

type StatusOfPlaying
    = Play
    | Pause
    | Waiting
    | BonusSelection
    | NextStage
    | Error
    | ErrorBonus


type Life
    = Dead

type Stages
    = Stage1
    | Stage2
    | Stage3
    | Stage4
    | Stage5


type alias Flags =
    ()


type alias Model =
    { ball : List Ball
    , brick : List Brick
    , paddle : Paddle
    , paddleMove : PaddleMove
    , gameStatus : GameStatus
    , score : Int
    , score_init : Int
    , leftWall : Wall
    , rightWall : Wall
    , life : Int
    , life_init : Int
    , stage : List Int
    , stageStatus : Stages --for displaying stage message purpose
    , twisttime : Int -- for twist purpose
    , longertime : Int
    , shortertime : Int
    , transparenttime : Int
    , biggertime : Int
    , bonuslist : List Int
    , bonusleft : List Int
    , fireballactivation : Bool
    , fireballtime : Int
    , extraballactivation : Bool
    , laseractivation : Bool
    , laserfire : Bool
    , laserball : List Ball
    , time : Int
    , messageTime : Int
    , items : List Item
    , seed : Random.Seed
    }

type alias Item =
    { x : Float
    , y : Float
    , effect : Int
    }

type alias Ball =
    { x : Float
    , y : Float
    , r : Float
    , horizontalSpeed : Float
    , verticalSpeed : Float
    , transparent : Bool
    }


type alias Brick =
    { position : ( Float, Float )
    , status : Int
    }


type alias Paddle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }

type alias Wall =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }

type alias Brickfunction =
    { twist : Bool
    , long : Bool
    , short : Bool
    , extralife : Bool
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Change stage to room name later
encodeStage : Stages -> String
encodeStage stage =
    case stage of
        Stage1 ->
            "Entrance Corridor"
        Stage2 ->
            "Antechamber"
        Stage3 ->
            "Annex"
        Stage4 ->
            "Treasury"
        Stage5 ->
            "Burial Chamber"

--Stage1 == Entrance Corridor
--State2 == Antechamber
--Stage3 == Annex
--Stage4 == Treasury
--stage5 == Burial Chamber
----------------------------------------------------------------------------------------------------------------


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { ball = initBall1
      , brick = initBricks0
      , paddle = initPaddle
      , paddleMove = NotMoving
      , gameStatus = Start
      , score = 0
      , score_init = 0
      , leftWall = initLeftWall
      , rightWall = initRightWall
      , life = 8
      , life_init = 8
      , stage = initStage
      , stageStatus = Stage1
      , twisttime = 0
      , longertime = 0
      , shortertime = 0
      , transparenttime = 0
      , biggertime = 0
      , bonuslist = []
      , bonusleft = initBonus
      , fireballactivation = False
      , fireballtime = 0
      , extraballactivation = False
      , laseractivation = False
      , laserfire = False
      , laserball = []
      , time = 0
      , messageTime = 0
      , items = []
      , seed = Random.initialSeed 0
      }
    , Cmd.none
    )


listBonus : List (List Int)
listBonus =
    [[1,2,3],[1,2,4],[1,2,5],[1,3,2],[1,3,4],[1,3,5],[1,4,2],[1,4,3],[1,4,5],[1,5,2],[1,5,3],[1,5,4]
    ,[2,1,3],[2,1,4],[2,1,5],[2,3,1],[2,3,4],[2,3,5],[2,4,1],[2,4,3],[2,4,5],[2,5,1],[2,5,3],[2,5,4]
    ,[3,1,2],[3,1,4],[3,1,5],[3,2,1],[3,2,4],[3,2,5],[3,4,1],[3,4,2],[3,4,5],[3,5,1],[3,5,2],[3,5,4]
    ,[4,1,2],[4,1,3],[4,1,5],[4,2,1],[4,2,3],[4,2,5],[4,3,1],[4,3,2],[4,3,5],[4,5,1],[4,5,2],[4,5,3]
    ,[5,1,2],[5,1,3],[5,1,4],[5,2,1],[5,2,3],[5,2,4],[5,3,1],[5,3,2],[5,3,4],[5,4,1],[5,4,2],[5,4,3]
    ]

initBonus : List Int
initBonus =
    [1,2,3,4,5]

initBall1 : List Ball
initBall1 =
    [Ball 495 572 8 3 5 False]

initBall2 : List Ball
initBall2 =
    [Ball 495 572 6.5 3 5 False, Ball 495 572 8 2.5 5 False]          --You can change the init ball here!!!


initPaddle : Paddle
initPaddle =
    Paddle 435 580 120 10

initStage : List Int
initStage = [1,2,3,4,5]

initBricks0 : List Brick
initBricks0 =
    []


initBricks1 : List Brick
initBricks1 =
     [    { position = ( 55, 298 ) , status = 1 }
        , { position = ( 136, 298 ) , status = 1 }
        , { position = ( 217, 298 ) , status = 1 }
        , { position = ( 298, 298 ) , status = 1 }
        , { position = ( 379, 298 ) , status = 2 }
        , { position = ( 460, 298 ) , status = 4 }
        , { position = ( 541, 298 ) , status = 2 }
        , { position = ( 622, 298 ) , status = 1 }
        , { position = ( 703, 298 ) , status = 1 }
        , { position = ( 784, 298 ) , status = 1 }
        , { position = ( 865, 298 ) , status = 1 }
        , { position = ( 55, 267 ) , status = 0 }
        , { position = ( 136, 267 ) , status = 0 }
        , { position = ( 136, 236 ) , status = 1 }
        , { position = ( 217, 236 ) , status = 0 }
        , { position = ( 217, 205 ) , status = 1 }
        , { position = ( 298, 174 ) , status = 3 }
        , { position = ( 298, 205 ) , status = 0 }
        , { position = ( 379, 267 ) , status = 0 }
        , { position = ( 379, 236 ) , status = 5 }
        , { position = ( 379, 205 ) , status = 0 }
        , { position = ( 379, 174 ) , status = 0 }
        , { position = ( 379, 143 ) , status = 7 }
        , { position = ( 865, 267 ) , status = 0 }
        , { position = ( 784, 267 ) , status = 0 }
        , { position = ( 784, 236 ) , status = 1 }
        , { position = ( 703, 236 ) , status = 0 }
        , { position = ( 703, 205 ) , status = 1 }
        , { position = ( 622, 174 ) , status = 3 }
        , { position = ( 622, 205 ) , status = 0 }
        , { position = ( 541, 267 ) , status = 0 }
        , { position = ( 541, 236 ) , status = 5 }
        , { position = ( 541, 205 ) , status = 0 }
        , { position = ( 541, 174 ) , status = 0 }
        , { position = ( 541, 143 ) , status = 7 }

        ]


initBricks2 : List Brick
initBricks2 =
    [ { position = ( 217, 298 ) , status = 3 }
        , { position = ( 217, 50 ) , status = 2 }
        , { position = ( 136, 298 ) , status = 3 }
        , { position = ( 136, 267 ) , status = 1 }
        , { position = ( 136, 50 ) , status = 2 }
        , { position = ( 136, 81 ) , status = 0 }
        , { position = ( 55, 298 ) , status = 3 }
        , { position = ( 55, 267 ) , status = 1 }
        , { position = ( 55, 236 ) , status = 4 }
        , { position = ( 55, 50 ) , status = 2 }
        , { position = ( 55, 81 ) , status = 0 }
        , { position = ( 55, 112 ) , status = 7 }
        , { position = ( 379, 298 ) , status = 2 }
        , { position = ( 460, 298 ) , status = 2 }
        , { position = ( 541, 298 ) , status = 2 }
        , { position = ( 379, 50 ) , status = 2 }
        , { position = ( 460, 50 ) , status = 2 }
        , { position = ( 541, 50 ) , status = 2 }
        , { position = ( 379, 81 ) , status = 1 }
        , { position = ( 379, 112 ) , status = 1 }
        , { position = ( 379, 143 ) , status = 0 }
        , { position = ( 379, 174 ) , status = 5 }
        , { position = ( 379, 205 ) , status = 0 }
        , { position = ( 379, 236 ) , status = 1 }
        , { position = ( 379, 267 ) , status = 1 }
        , { position = ( 541, 81 ) , status = 1 }
        , { position = ( 541, 112 ) , status = 1 }
        , { position = ( 541, 143 ) , status = 0 }
        , { position = ( 541, 174 ) , status = 5 }
        , { position = ( 541, 205 ) , status = 0 }
        , { position = ( 541, 236 ) , status = 1 }
        , { position = ( 541, 267 ) , status = 1 }
        , { position = ( 703, 298 ) , status = 3 }
        , { position = ( 703, 50 ) , status = 2 }
        , { position = ( 784, 298 ) , status = 3 }
        , { position = ( 784, 267 ) , status = 1 }
        , { position = ( 784, 50 ) , status = 2 }
        , { position = ( 784, 81 ) , status = 0 }
        , { position = ( 865, 298 ) , status = 3 }
        , { position = ( 865, 267 ) , status = 1 }
        , { position = ( 865, 236 ) , status = 7 }
        , { position = ( 865, 50 ) , status = 2 }
        , { position = ( 865, 81 ) , status = 0 }
        , { position = ( 865, 112 ) , status = 6 }
        ]


initBricks3 : List Brick
initBricks3 =
    [ { position = ( 55, 298 ) , status = 1 }--col1
        , { position = ( 55, 267 ) , status = 2 }
        , { position = ( 55, 236 ) , status = 0 }
        , { position = ( 55, 205 ) , status = 0 }
        , { position = ( 55, 174 ) , status = 5 }
        , { position = ( 55, 143 ) , status = 0 }
        , { position = ( 55, 112 ) , status = 0 }
        , { position = ( 55, 81 ) , status = 2 }
        , { position = ( 55, 50 ) , status = 1 }
        , { position = ( 298, 298 ) , status = 3 }--col3
        , { position = ( 298, 267 ) , status = 0 }
        , { position = ( 298, 236 ) , status = 0 }
        , { position = ( 298, 205 ) , status = 1 }
        , { position = ( 298, 174 ) , status = 0 }
        , { position = ( 298, 143 ) , status = 2 }
        , { position = ( 298, 143 ) , status = 7 }
        , { position = ( 379, 298 ) , status = 3 }--col4
        , { position = ( 379, 267 ) , status = 0 }
        , { position = ( 379, 236 ) , status = 0 }
        , { position = ( 379, 205 ) , status = 2 }
        , { position = ( 460, 298 ) , status = 4 }--col5
        , { position = ( 460, 267 ) , status = 5 }
        , { position = ( 460, 81 ) , status = 0 }
        , { position = ( 460, 50 ) , status = 6 }
        , { position = ( 541, 298 ) , status = 3 }--col6
        , { position = ( 541, 267 ) , status = 0 }
        , { position = ( 541, 236 ) , status = 0 }
        , { position = ( 541, 205 ) , status = 2 }
        , { position = ( 622, 298 ) , status = 3 }--col7
        , { position = ( 622, 267 ) , status = 0 }
        , { position = ( 622, 236 ) , status = 0 }
        , { position = ( 622, 205 ) , status = 1 }
        , { position = ( 622, 174 ) , status = 0 }
        , { position = ( 622, 143 ) , status = 2 }
        , { position = ( 622, 143 ) , status = 7 }
        , { position = ( 865, 298 ) , status = 1 }--col9
        , { position = ( 865, 267 ) , status = 2 }
        , { position = ( 865, 236 ) , status = 0 }
        , { position = ( 865, 205 ) , status = 0 }
        , { position = ( 865, 174 ) , status = 5 }
        , { position = ( 865, 143 ) , status = 0 }
        , { position = ( 865, 112 ) , status = 0 }
        , { position = ( 865, 81 ) , status = 2 }
        , { position = ( 865, 50 ) , status = 1 }
        ]


initBricks4 : List Brick
initBricks4 =
    [ { position = ( 55, 298 ) , status = 1 }--col1
    , { position = ( 55, 267 ) , status = 0 }
    , { position = ( 55, 50 ) , status = 1 }
    , { position = ( 55, 81 ) , status = 0 }
    , { position = ( 136, 298 ) , status = 1 }--col2
    , { position = ( 136, 267 ) , status = 0 }
    , { position = ( 136, 236 ) , status = 5 }
    , { position = ( 136, 205 ) , status = 0 }
    , { position = ( 136, 174 ) , status = 7 }
    , { position = ( 136, 143 ) , status = 0 }
    , { position = ( 136, 112 ) , status = 5 }
    , { position = ( 136, 81 ) , status = 0 }
    , { position = ( 136, 50 ) , status = 1}
    , { position = ( 217, 298 ) , status = 1 }--col3
    , { position = ( 217, 50 ) , status = 1 }
    , { position = ( 298, 298 ) , status = 2 }--col4
    , { position = ( 298, 267 ) , status = 0 }
    , { position = ( 298, 236 ) , status = 0 }
    , { position = ( 379, 298 ) , status = 2 }--col5
    , { position = ( 379, 267 ) , status = 1 }
    , { position = ( 379, 236 ) , status = 3 }
    , { position = ( 379, 205 ) , status = 0 }
    , { position = ( 379, 174 ) , status = 0 }
    , { position = ( 460, 298 ) , status = 3 }--col6
    , { position = ( 460, 267 ) , status = 1 }
    , { position = ( 460, 236 ) , status = 1 }
    , { position = ( 460, 205 ) , status = 4 }
    , { position = ( 460, 174 ) , status = 0 }
    , { position = ( 460, 143 ) , status = 6 }
    , { position = ( 541, 298 ) , status = 2 }--col7
    , { position = ( 541, 267 ) , status = 1 }
    , { position = ( 541, 236 ) , status = 3 }
    , { position = ( 541, 205 ) , status = 0 }
    , { position = ( 541, 174 ) , status = 0 }
    , { position = ( 622, 298 ) , status = 2 }--col8
    , { position = ( 622, 267 ) , status = 0 }
    , { position = ( 622, 236 ) , status = 0 }
    , { position = ( 703, 298 ) , status = 1 }--col9
    , { position = ( 703, 50 ) , status = 1 }
    , { position = ( 784, 298 ) , status = 1 }--col10
    , { position = ( 784, 267 ) , status = 0 }
    , { position = ( 784, 236 ) , status = 5 }
    , { position = ( 784, 205 ) , status = 0 }
    , { position = ( 784, 174 ) , status = 7 }
    , { position = ( 784, 143 ) , status = 0 }
    , { position = ( 784, 112 ) , status = 5 }
    , { position = ( 784, 81 ) , status = 0 }
    , { position = ( 784, 50 ) , status = 1}
    , { position = ( 865, 298 ) , status = 1 }--col11
    , { position = ( 865, 267 ) , status = 0 }
    , { position = ( 865, 50 ) , status = 1 }
    , { position = ( 865, 81 ) , status = 0 }
    ]

initBricks5 : List Brick
initBricks5 =
    [  { position = ( 55, 236 ) , status = 2 }--col1
    , { position = ( 55, 205 ) , status = 5 }
    , { position = ( 55, 174 ) , status = 0 }
    , { position = ( 55, 143 ) , status = 5 }
    , { position = ( 55, 112 ) , status = 0 }
    , { position = ( 55, 81 ) , status = 5 }
    , { position = ( 136, 267 ) , status = 2 }--col2
    , { position = ( 136, 236 ) , status = 0 }
    , { position = ( 136, 205 ) , status = 0 }
    , { position = ( 136, 174 ) , status = 2 }
    , { position = ( 217, 298 ) , status = 2 }--col3
    , { position = ( 217, 267 ) , status = 0 }
    , { position = ( 298, 298 ) , status = 3 }--col4
    , { position = ( 298, 267 ) , status = 0 }
    , { position = ( 379, 298 ) , status = 3 }--col5
    , { position = ( 379, 267 ) , status = 1 }
    , { position = ( 379, 236 ) , status = 0 }
    , { position = ( 379, 205 ) , status = 0 }
    , { position = ( 379, 174 ) , status = 0 }
    , { position = ( 379, 143 ) , status = 6 }
    , { position = ( 460, 298 ) , status = 3 }--col6
    , { position = ( 460, 267 ) , status = 1 }
    , { position = ( 460, 236 ) , status = 0 }
    , { position = ( 460, 205 ) , status = 7 }
    , { position = ( 460, 174) , status = 0 }
    , { position = ( 460, 143 ) , status = 4 }
    , { position = ( 541, 298 ) , status = 3 }--col7
    , { position = ( 541, 267 ) , status = 1 }
    , { position = ( 541, 236 ) , status = 0 }
    , { position = ( 541, 205 ) , status = 0 }
    , { position = ( 541, 174 ) , status = 0 }
    , { position = ( 541, 143 ) , status = 6 }
    , { position = ( 622, 298 ) , status = 3 }--col8
    , { position = ( 622, 267 ) , status = 0 }
    , { position = ( 703, 298 ) , status = 2 }--col9
    , { position = ( 703, 267 ) , status = 0 }
    , { position = ( 784, 267 ) , status = 2 }--col10
    , { position = ( 784, 236 ) , status = 0 }
    , { position = ( 784, 205 ) , status = 0 }
    , { position = ( 784, 174 ) , status = 2 }
    , { position = ( 865, 236 ) , status = 2 }--col11
    , { position = ( 865, 205 ) , status = 5 }
    , { position = ( 865, 174 ) , status = 0 }
    , { position = ( 865, 143 ) , status = 5 }
    , { position = ( 865, 112 ) , status = 0 }
    , { position = ( 865, 81 ) , status = 5 }
    ]




initLeftWall : Wall
initLeftWall =
    Wall 54 8 130 35

initRightWall : Wall
initRightWall =
    Wall 966 59 160 110


----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =

    div []
        [svg
              [ width "1150"
              , height "600"
              , viewBox "0 0 1150 600"
              , Svg.Attributes.style "background: #dedede"
              ]
              (List.append
              (List.append
              (List.append
              (List.append
              (List.append
              [dispBackground --This box will be under white screen
              , dispWhite
              , dispWhite2 model
              , dispCeiling
              , dispRightWall
              , dispLeftWall
              , displayScoreBox model
              , displayStatusBox model
              , displayMessageBox model
              ]
              [ dispRamses model  --This box wil not be under the white screen
              , dispStage1 model
              , dispStage2 model
              , dispStage3 model
              , dispStage4 model
              , dispStage5 model
              , statusMessage model
              , displayPaddle model.paddle]
               )
               (displayBricks model.brick)
               )
               (displayItems model.items)
               )
               (displayBall model.ball)
               )
               (displayBall model.laserball)
               )
        , displayMainButton1 model
        , displayMainButton2 model
        , stage1Button model
        , stage2Button model
        , stage3Button model
        , stage4Button model
        , stage5Button model
        , bonus1Button model
        , bonus2Button model
        , bonus3Button model
        , bonus4Button model
        , bonus5Button model
        ]



--ceiling-- https://s1.ax1x.com/2020/06/16/NASIbT.jpg
--left wall -- https://s1.ax1x.com/2020/06/16/NApgeK.jpg
-- right wal -- https://s1.ax1x.com/2020/06/16/NAp4Wd.jpg
dispBackground : Svg.Svg Msg
dispBackground =
   Svg.image
      [ x "54"
      , y "49"
      , width "892"
      , height "551"
      , xlinkHref "https://s1.ax1x.com/2020/06/17/NAPkRJ.jpg"
      ][]
      --
dispRamses : Model -> Svg.Svg Msg
dispRamses model =
    if model.gameStatus == Start then
        Svg.image
              [ x "600"
              , y "150"
              , width "200"
              , height "200"
              , xlinkHref "https://s1.ax1x.com/2020/06/17/NAVhY6.png"
              ][]
    else
        Svg.rect
              [][]

dispCeiling : Svg.Svg Msg
dispCeiling =
   Svg.image
      [ x "0"
      , y "0"
      , width "1150"
      , height "49"
      , xlinkHref "https://s1.ax1x.com/2020/06/16/NASIbT.jpg"
      ][]

dispRightWall : Svg.Svg Msg
dispRightWall=
   Svg.image
      [ x "946"
      , y "0"
      , width "204"
      , height "600"
      , xlinkHref "https://s1.ax1x.com/2020/06/16/NAp4Wd.jpg"
      ][]

dispLeftWall : Svg.Svg Msg
dispLeftWall=
   Svg.image
      [ x "0"
      , y "0"
      , width "54"
      , height "600"
      , xlinkHref "https://s1.ax1x.com/2020/06/16/NApgeK.jpg"
      ][]

dispWhite : Svg.Svg Msg
dispWhite =
    Svg.rect
        [ x "54"
        , y "49"
        , width  "892"
        , height "551"
        , fill "#e5e3e057"
        ]
        []
dispWhite2 : Model -> Svg.Svg Msg
dispWhite2 model=
    if model.gameStatus == (Playing <| NextStage) then
    Svg.rect
        [ x "65"
        , y "215"
        , width  "860"
        , height "120"
        , fill "#e5e3e0a3"
        ]
        []
    else Svg.rect [][]

displayScoreBox : Model -> Svg.Svg Msg
displayScoreBox model  =
    let
        wall = model.rightWall
    in
    case model.gameStatus of
        Playing status ->
            case status of
                Play ->
                    Svg.rect
                        [ x <| String.fromInt wall.x
                         , y <| String.fromInt wall.y
                        , width <| String.fromInt wall.width
                        , height <| String.fromInt wall.height
                        , fill "#e5e3e0b7"
                        ]
                        []

                Pause ->
                    Svg.rect
                       [ x <| String.fromInt wall.x
                       , y <| String.fromInt wall.y
                       , width <| String.fromInt wall.width
                       , height <| String.fromInt wall.height
                       , fill "#e5e3e0b7"
                       ]
                       []
                _ ->
                    Svg.rect
                       [][]

        _ ->
            Svg.rect
                    [][]

displayStatusBox : Model -> Svg.Svg Msg
displayStatusBox model  =
    let
        wall = model.leftWall
    in
    case model.gameStatus of
        Playing status ->
            case status of
                Play ->
                    Svg.rect
                        [ x <| String.fromInt wall.x
                         , y <| String.fromInt wall.y
                        , width <| String.fromInt wall.width
                        , height <| String.fromInt wall.height
                        , fill "#e5e3e0b7"
                        ]
                        []

                Pause ->
                    Svg.rect
                       [ x <| String.fromInt wall.x
                       , y <| String.fromInt wall.y
                       , width <| String.fromInt wall.width
                       , height <| String.fromInt wall.height
                       , fill "#e5e3e0b7"
                       ]
                       []
                _ ->
                    Svg.rect
                       [][]

        _ ->
            Svg.rect
                    [][]


displayMessageBox : Model -> Svg.Svg Msg
displayMessageBox model =
    if needToPrintActiveTime model && model.gameStatus == (Playing<|Play) then
        Svg.rect
           [ x "375"
           , y "8"
           , width  "245"
           , height "35"
           , fill "#e5e3e0ce"
           ]
           []

    else
        Svg.rect
           [][]

dispStage1 : Model -> Svg.Svg Msg
dispStage1 model =
    if stageA 1 model then
        Svg.image
          [ x "80"
          , y "200"
          , width "150"
          , height "150"
          , xlinkHref "https://s1.ax1x.com/2020/06/17/NEMROK.png"
          ][]
    else
        Svg.rect[][]

dispStage2 : Model -> Svg.Svg Msg
dispStage2 model =
    if stageA 2 model then
        Svg.image
          [ x "250"
          , y "200"
          , width "150"
          , height "150"
          , xlinkHref "https://s1.ax1x.com/2020/06/17/NEQ0jP.png"
          ][]
    else
        Svg.rect[][]

dispStage3 : Model -> Svg.Svg Msg
dispStage3 model =
    if stageA 3 model then
        Svg.image
          [ x "420"
          , y "200"
          , width "150"
          , height "150"
          , xlinkHref "https://s1.ax1x.com/2020/06/17/NEQsHS.png"
          ][]
    else
        Svg.rect[][]

dispStage4 : Model -> Svg.Svg Msg
dispStage4 model =
    if stageA 4 model then
        Svg.image
          [ x "590"
          , y "200"
          , width "150"
          , height "150"
          , xlinkHref "https://s1.ax1x.com/2020/06/17/NEQR9s.png"
          ][]
    else
        Svg.rect[][]

dispStage5 : Model -> Svg.Svg Msg
dispStage5 model =
    if stageA 5 model then
        Svg.image
          [ x "760"
          , y "200"
          , width "150"
          , height "150"
          , xlinkHref "https://s1.ax1x.com/2020/06/17/NEQIBT.png"
          ][]
    else
        Svg.rect[][]


--check if we should display stage button or not
stageA : Int -> Model -> Bool
stageA number model =
    checkStageAvailable number model.stage  && (model.gameStatus == (Playing <| NextStage))

bonusA : Int -> Model -> Bool
bonusA number model =
    checkBonusAvailable number model.bonusleft && (model.gameStatus == (Playing <| BonusSelection))


--Main control button
displayMainButton1 : Model -> Html Msg
displayMainButton1 model =
            let
                (txt ,(x,y),msg) =
                     case model.gameStatus of
                         Start ->
                             ("Start", ("970px" , "450px"), GenerateRandomNumber )
                         Playing status ->
                              case status of
                              Play ->
                                 ("Pause", ("970px" , "450px"), ButtonPressed (Change (Playing <|Pause) model.stageStatus))
                              Pause ->
                                 ("Resume", ("970px" , "450px") , ButtonPressed (Change (Playing <|Play) model.stageStatus))
                              Waiting ->
                                 ("Play",  ("970px" , "450px") , ButtonPressed (Change (Playing <|Play) model.stageStatus))
                              Error ->
                                 ("Return to Select", ("970px","520px") , ButtonPressed (Change (Playing <|NextStage) model.stageStatus))
                              ErrorBonus ->
                                 ("Return to Select", ("970px","520px") , ButtonPressed (Change (Playing <|BonusSelection) model.stageStatus))
                              _ ->
                                 ("", ("-300px" , "-300") , Backup)
                         Died ->
                             ("", ("-100px" , "-100") , Backup)
                         Win ->
                                ("Return to Main Menu", ("970px","520px") , ButtonPressed (Reset (Start)))
                         Lose ->
                                    ("Return to Main Menu", ("970px","520px") ,ButtonPressed (Reset (Start)))
                (width,height) =
                    case model.gameStatus of
                        Win -> ("160px","60px")
                        Lose -> ("160px","60px")
                        Start -> ("160px","60px")
                        Playing _ ->
                            ("160px","60px")
                        Died  -> ("0px","0px")
                fontSize =
                    case model.gameStatus of
                        Start -> "25px"
                        _ -> "18px"


            in
            makeButton txt (x,y) (width,height)  "#1B358E" (fontSize, "#ffffff") msg

--Main Button2
displayMainButton2 : Model -> Html Msg
displayMainButton2 model =
            let
                (txt , (x,y), msg) =
                     case model.gameStatus of
                         Start ->
                             ("Select Stage", ("970px","520px") , ButtonPressed (Change (Playing <|NextStage) model.stageStatus))
                         Playing status ->
                              case status of
                              Pause ->
                                 ("Return to Main Menu",  ("970px","520px") , ButtonPressed (Change (Start) model.stageStatus))
                              Waiting ->
                                 ("Return to Main Menu",  ("970px","520px") , ButtonPressed (Change (Start) model.stageStatus))
                              _ ->
                                 ("",("-100px","-100px"),Backup)
                         _ ->
                             ("", ("-100px","-100px") , Backup)

                (width,height) =
                      ("160px","60px")

                fontSize =
                      case model.gameStatus of
                            Start -> "25px"
                            _ -> "18px"
            in
            makeButton txt (x,y) (width,height)  "#1B358E" (fontSize, "#ffffff") msg

stage1Button : Model -> Html Msg
stage1Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|NextStage) then
                 ("Entrance Corridor", ("970px","200px") , ChooseStage(Stage1))
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|NextStage) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if stageA 1 model then
                "#1B358E"

            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

stage2Button : Model -> Html Msg
stage2Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|NextStage) then
                 ("Antechamber", ("970px","260px") , ChooseStage(Stage2))
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|NextStage) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if stageA 2 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

stage3Button : Model -> Html Msg
stage3Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|NextStage) then
                 ("Annex", ("970px","320px") , ChooseStage(Stage3))
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|NextStage) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if stageA 3 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

stage4Button : Model -> Html Msg
stage4Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|NextStage) then
                 ("Treasury", ("970px","380px") , ChooseStage(Stage4))
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|NextStage) then
                  ("160px","40px")
             else
                  ("0px","0px")

        color =
            if stageA 4 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg


stage5Button : Model -> Html Msg
stage5Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|NextStage) then
                 ("Burial Chamber", ("970px","440px") , ChooseStage(Stage5))
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|NextStage) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if stageA 5 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

bonus1Button : Model -> Html Msg
bonus1Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|BonusSelection) then
                 ("Treasure 1", ("970px","200px") , ChooseBonus 1)
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|BonusSelection) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if bonusA 1 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

bonus2Button : Model -> Html Msg
bonus2Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|BonusSelection) then
                 ("Treasure 2", ("970px","260px") , ChooseBonus 2)
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|BonusSelection) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if bonusA 2 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

bonus3Button : Model -> Html Msg
bonus3Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|BonusSelection) then
                 ("Treasure 3", ("970px","320px") , ChooseBonus 3)
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|BonusSelection) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if bonusA 3 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

bonus4Button : Model -> Html Msg
bonus4Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|BonusSelection) then
                 ("Treasure 4", ("970px","380px") , ChooseBonus 4)
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|BonusSelection) then
                  ("160px","40px")
             else
                  ("0px","0px")

        color =
            if bonusA 4 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg


bonus5Button : Model -> Html Msg
bonus5Button model  =
    let
        (txt , (x,y), msg) =
            if model.gameStatus == (Playing<|BonusSelection) then
                 ("Treasure 5", ("970px","440px") , ChooseBonus 5)
            else
                ("", ("-100px","-100px") , Backup)

        (width,height) =
             if model.gameStatus == (Playing<|BonusSelection) then
                  ("160px","40px")
             else
                  ("0px","0px")
        color =
            if bonusA 5 model then
                "#1B358E"
            else
                "#740505"
    in
     makeButton txt (x,y) (width,height) color ("18px", "#ffffff") msg

makeButton : String -> (String, String) -> (String, String) -> String -> (String,String) -> Msg -> Html Msg
makeButton txt (x,y) (width,height) bgcolor (fontSize,fontColor) msg=
             button
                 [ Html.Attributes.style "background" bgcolor
                 , Html.Attributes.style "border" "0"
                 , Html.Attributes.style "top" y
                 , Html.Attributes.style "color" fontColor
                 , Html.Attributes.style "cursor" "pointer"
                 , Html.Attributes.style "display" "block"
                 , Html.Attributes.style "font-family" "Helvetica, Arial, sans-serif"
                 , Html.Attributes.style "font-size" fontSize
                 , Html.Attributes.style "font-weight" "300"
                 , Html.Attributes.style "height" height
                 , Html.Attributes.style "left" x
                 , Html.Attributes.style "line-height" "20px"
                 , Html.Attributes.style "outline" "none"
                 , Html.Attributes.style "padding" "0"
                 , Html.Attributes.style "position" "absolute"
                 , Html.Attributes.style "width" width
                 , onClick msg
                 ]
                 [Html.text txt]


displayABall : Ball -> Svg.Svg Msg
displayABall ball=
    if ball.transparent then
    Svg.circle
        [ cx <| String.fromFloat ball.x
        , cy <| String.fromFloat ball.y
        , r <| String.fromFloat ball.r
        , fill "#e81b2b17"
        ]
        []
    else
    Svg.circle
        [ cx <| String.fromFloat ball.x
        , cy <| String.fromFloat ball.y
        , r <| String.fromFloat ball.r
        , fill "#e82b2b"
        ]
        []



displayBall : List Ball -> List ( Svg.Svg Msg )
displayBall balls =
    List.map displayABall balls


makeBrick : Brick -> Svg.Svg Msg
makeBrick brick =
    case brick.status of
        --extra life
        7 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#679008"
            ]
            []

        --short
        6 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#581845"
            ]
            []

        --long
        5 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
             , y <| String.fromFloat (brick.position |> Tuple.second)
             , width "80"
             , height "30"
             , fill "#085190"
             ]
             []

        --twist
        4 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#740505"
            ]
            []

        3 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#905108"
            ]
            []

        2 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#b8670b"
            ]
            []
        1 -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#e08215"
            ]
            []

        _ -> Svg.rect
            [ x <| String.fromFloat (brick.position |> Tuple.first)
            , y <| String.fromFloat (brick.position |> Tuple.second)
            , width "80"
            , height "30"
            , fill "#ffb52b"
            ]
            []

makeItem : Item -> Svg.Svg Msg
makeItem item =
    case item.effect of
        0 ->        -- effect: shorter paddle
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#d94040"    --#d94040
                ]
                []
        1 ->        -- effect: longer paddle
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#66CC99"    --#666666
                ]
                []
        2 ->        -- effect: twist paddle
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#d94040"    --#6abb24
                ]
                []
        3 ->        -- effect: bigger ball
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#66CC99"    --#520505
                ]
                []
        4 ->        -- effect: transparent pall
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#d94040"    --#10d9c2
                ]
                []
        _ ->        -- effect: none
            Svg.rect
                [ x <| String.fromFloat item.x
                , y <| String.fromFloat item.y
                , width  "20"
                , height "20"
                , fill "#404040"    --#40404020
                ]
                []




displayBricks : List Brick -> List (Svg.Svg Msg)
displayBricks brick =
    List.map makeBrick brick

displayItems : List Item -> List (Svg.Svg Msg)
displayItems items =
    List.map makeItem items


displayPaddle : Paddle -> Svg.Svg Msg
displayPaddle paddle =
    Svg.rect
        [ x <| String.fromFloat paddle.x
        , y <| String.fromFloat paddle.y
        , width <| String.fromFloat paddle.width
        , height <| String.fromFloat paddle.height
        , fill "#1B358E"
        ]
        []

--edit messgae with roomname later
statusMessage : Model -> Svg.Svg Msg
statusMessage model =
    case model.gameStatus of
        Playing status ->
            case status of
                Play ->
                    g
                        [ fontSize "20px", fontFamily "monospace" ]
                        [ text_ [ x "60", y "30", textAnchor "start" ] [ text "Playing..." ]
                        , text_ [ x "990", y "100", textAnchor "start" ] [ text "Life: " ]
                        , text_ [ x "1080", y "100", textAnchor "end" ] [ text <| String.fromInt model.life ]
                        , text_ [ x "990", y "140", textAnchor "start" ] [ text "Score: " ]
                        , text_ [ x "1070", y "140", textAnchor "start" ] [ text <| String.fromInt model.score ]
                        , text_ [ x "500", y "400", textAnchor "middle" ][text (printItem model) ]
                        , text_ [ x "500", y "30", textAnchor "middle" ][text (printTime model) ]
                        ]

                Pause ->
                    g
                        [ fontSize "20px", fontFamily "monospace" ]
                        [ text_ [ x "60", y "30", textAnchor "start" ] [ text "Paused" ]
                        , text_ [ x "990", y "100", textAnchor "start" ] [ text "Life: " ]
                        , text_ [ x "1080", y "100", textAnchor "end" ] [ text <| String.fromInt model.life ]
                        , text_ [ x "990", y "140", textAnchor "start" ] [ text "Score: " ]
                        , text_ [ x "1070", y "140", textAnchor "start" ] [ text <| String.fromInt model.score ]
                        ]

                Waiting ->
                    g
                       [ fontSize "30px", fontFamily "monospace" ]
                       [ text_ [ x "500", y "370", textAnchor "middle" ] [ text (encodeStage (model.stageStatus))]
                       , text_ [ x "500", y "400", textAnchor "middle" ] [ text ("Bonus for this Stage: " ++ printBonus model)]
                       , text_ [ x "280", y "430", textAnchor "start" ] [ text "Click the button to Start"]
                       ]

                NextStage ->
                    g
                       [ fontSize "23px", fontFamily "monospace" ]
                       [ text_ [ x "500", y "205", textAnchor "middle" ] [ text "Stage Available: " ]
                       , text_ [ x "500", y "360", textAnchor "middle" ] [ text (printAvailableStage model) ]
                       , text_ [ x "500", y "410", textAnchor "middle" ] [ text ("Bonus for this Stage: " ++ printBonus model)]
                       , text_ [ x "500", y "460", textAnchor "middle" ] [ text ("Click the button to choose next stage") ]
                       ]


                Error ->
                    g
                       [ fontSize "30px", fontFamily "monospace" ]
                       [ text_ [ x "500", y "300", textAnchor "middle" ] [ text ("You have played this stage") ]
                       , text_ [ x "500", y "350", textAnchor "middle" ] [ text ("Please choose a new stage") ]
                       ]

                ErrorBonus ->
                    g
                       [ fontSize "30px", fontFamily "monospace" ]
                       [ text_ [ x "500", y "300", textAnchor "middle" ] [ text ("You have selected this treasure") ]
                       , text_ [ x "500", y "350", textAnchor "middle" ] [ text ("Please choose a new one") ]
                       ]

                BonusSelection ->
                    g
                       [ fontSize "30px", fontFamily "monospace" ]
                       [ text_ [ x "500", y "300", textAnchor "middle" ] [ text ("You have the chance to collect a treasure") ]
                       , text_ [ x "500", y "350", textAnchor "middle" ] [ text ("Please choose a treasure") ]
                       ]

        Died ->
            g
                [ fontSize "40px", fontFamily "monospace" ]
                [ text_ [ x "500", y "400", textAnchor "middle" ] [ text "Respawning..." ]
                ]


        Win ->
            g
                [ fontSize "29px", fontFamily "monospace" ]
                [ text_ [ x "500", y "300", textAnchor "middle" ] [ text "Congratulations!" ]
                , text_ [ x "500", y "350", textAnchor "middle" ] [ text "You successfully helped Seth to achieve his dream." ]
                ]


        Lose ->
            g
                [ fontSize "50px", fontFamily "monospace" ]
                [ text_ [ x "500", y "400", textAnchor "middle" ] [ text "Seth has died, Try Again!" ]
                ]

        _ ->
            g
                [ fontSize "50px", fontFamily "monospace" ]
                [ text_ [ x "500", y "400", textAnchor "middle" ] [ text "The Lost Tomb of Ramses VIII" ]
                ]

--change the number to room name
printAvailableStage: Model -> String
printAvailableStage model =
    let
        stage =
            model.stage
        stage1 =
            if List.member 1 stage then
            "Entrance Corridor/ "
            else ""
        stage2 =
             if List.member 2 stage then
             "Antechamber/ "
             else ""
        stage3 =
            if List.member 3 stage then
            "Annex/ "
            else ""
        stage4 =
            if List.member 4 stage then
            "Treasury/ "
            else ""
        stage5 =
            if List.member 5 stage then
            "Burial Chamber"
            else ""
    in
    stage1++stage2++stage3++stage4++stage5

printBonus : Model -> String
printBonus model =
    if model.extraballactivation then
    "Extra Ball"
    else if model.fireballactivation then
    "Fire Ball"
    else if model.laseractivation then
    "Laser Ball"
    else
    "Nothing"



--message time
timeM : Model -> Bool
timeM model =
     if model.messageTime > 0 then
                    True
                else
                    False


printItem : Model -> String
printItem model =

    if model.twisttime >= 250 && model.twisttime <= 300 && timeM model then
        "Reverse Paddle Activated"
    else if model.longertime >= 550 && model.longertime <= 600 && timeM model then
        "Extend Paddle Activated"
    else if model.shortertime >= 550 && model.shortertime <= 600 && timeM model  then
        "Shorten Paddle Activated"
    else if model.transparenttime >= 550 && model.transparenttime <= 600 && timeM model then
        "Transparent Ball Activated"
    else if model.biggertime >= 550 && model.biggertime <= 600 && timeM model then
         "Bigger Ball Activated"
    else if timeM model then
        "Life +1"
    else ""



needToPrintActiveTime : Model -> Bool
needToPrintActiveTime model =
     model.twisttime >= 1 || model.longertime >= 1 || model.shortertime >= 1 || model.transparenttime >= 1 || model.biggertime >= 1



printTime : Model -> String
printTime model =
    let
       activation =
           if model.twisttime >= 1 || model.longertime >= 1 || model.shortertime >= 1 || model.transparenttime >= 1 || model.biggertime >= 1 then
           "Activation Time: "
           else ""
       time =
        if model.twisttime >= 1  then
            String.fromInt (model.twisttime // 60)
        else if model.longertime >= 1  then
            String.fromInt (model.longertime // 60)
        else if model.shortertime >= 1  then
            String.fromInt (model.shortertime // 60)
        else if model.transparenttime >= 1  then
            String.fromInt (model.transparenttime // 60)
        else if model.biggertime >= 1  then
              String.fromInt (model.biggertime // 60)
        else ""
    in
    activation ++ time

    --Stage1 == Entrance Corridor
    --State2 == Antechamber
    --Stage3 == Annex
    --Stage4 == Treasury
    --stage5 == Burial Chamber
----------------------------------------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Backup ->
            ( model , Cmd.none)

        NewRandomBonus number ->
            let
                bonuslist =
                    case model.bonuslist of
                        [] ->
                            Maybe.withDefault [1,2,3] (List.head (List.drop number listBonus))
                        _ ->
                            model.bonuslist
            in
            ( {model| bonuslist = bonuslist}, Cmd.none)

        GenerateRandomNumber ->
            let
                stage =
                    case model.stageStatus of
                        Stage1 -> 1
                        Stage2 -> 2
                        Stage3 -> 3
                        Stage4 -> 4
                        Stage5 -> 5
            in
            ( {model |  brick = initBricks1
             , stage = updateStage stage model}
             , Random.generate NewRandomNumber (Random.int 0 200) )

        NewRandomNumber number ->
            let
                brick_init =
                    case model.stageStatus of
                        Stage1 ->
                            initBricks1
                        Stage2 ->
                            initBricks2
                        Stage3 ->
                            initBricks3
                        Stage4 ->
                            initBricks4
                        Stage5 ->
                            initBricks5
            in
            ( { model | seed = Random.initialSeed (number), gameStatus = Playing <| Waiting , brick = brick_init} , Random.generate NewRandomBonus (Random.int 0 119))

        Animation _ ->
            let
                --updateBricks--------------------------
                ball =
                    model.ball

                laserball =
                    model.laserball

                new_model = calculateUpdate ball msg model []

                new_laser_model = calculateLaserUpdate laserball msg new_model []

                updatePaddle =
                            updatePaddleMove new_laser_model.twisttime new_laser_model.paddleMove new_laser_model.paddle new_laser_model.longertime new_laser_model.shortertime

                newball = new_model.ball

                newlaserball = new_laser_model.laserball

                gameStatus =
                    Tuple.first (updateCommandAndStatus msg new_laser_model newball)

                cmd =
                    Tuple.second (updateCommandAndStatus msg new_laser_model newball)

                score =
                    new_laser_model.score

                life = updateLife new_laser_model newball

                time =
                    if new_laser_model.time >= 600 then
                        0

                    else
                        new_laser_model.time + 1

                twisttime =
                    if new_laser_model.twisttime >= 1 then
                        new_laser_model.twisttime - 1

                    else
                        new_laser_model.twisttime

                longertime =
                    if new_laser_model.longertime >= 1 then
                        new_laser_model.longertime - 1

                    else
                        new_laser_model.longertime

                shortertime =
                    if new_laser_model.shortertime >= 1 then
                        new_laser_model.shortertime - 1

                    else
                        new_laser_model.shortertime

                transparenttime =
                    if new_laser_model.transparenttime >= 1 then
                        new_laser_model.transparenttime - 1

                    else
                        new_laser_model.transparenttime

                biggertime =
                    if new_laser_model.biggertime >= 1 then
                        new_laser_model.biggertime - 1

                    else
                        new_laser_model.biggertime

                fireballtime =
                    if new_laser_model.fireballtime >= 1 then
                        new_laser_model.fireballtime - 1
                    else
                        new_laser_model.fireballtime

                fireballactivation =
                    if new_laser_model.gameStatus == (Playing <| BonusSelection) then
                    False
                    else
                    new_laser_model.fireballactivation

                extraballactivation =
                    if new_laser_model.gameStatus == (Playing <| BonusSelection) then
                    False
                    else
                    new_laser_model.extraballactivation

                laseractivation =
                    if new_laser_model.gameStatus == (Playing <| BonusSelection) then
                    False
                    else
                    new_laser_model.laseractivation

                messageTime =
                    if new_laser_model.messageTime >= 1 then
                        new_laser_model.messageTime - 1

                    else new_laser_model.messageTime

                newestModel = { new_laser_model
                                | paddle = updatePaddle
                                , gameStatus = gameStatus
                                , score = score
                                , life = life
                                , time = time
                                , twisttime = twisttime
                                , longertime = longertime
                                , shortertime = shortertime
                                , transparenttime = transparenttime
                                , biggertime = biggertime
                                , fireballactivation = fireballactivation
                                , fireballtime = fireballtime
                                , extraballactivation = extraballactivation
                                , laseractivation = laseractivation
                                , laserball = newlaserball
                                , messageTime = messageTime
                                --, items = items
                                }

            in
            (newestModel, cmd)


        ButtonPressed playerCommand ->
            case playerCommand of
                MovePaddleLeft ->
                    ( { model | paddleMove = MoveLeft }, Cmd.none )

                --If the LeftArrow button was pressed, tell paddle to move left
                MovePaddleRight ->
                    ( { model | paddleMove = MoveRight }, Cmd.none )

                LaserFire ->
                    if model.laserfire then
                    let
                            ball1 = Ball model.paddle.x (model.paddle.y - 4) 4 0 -20 False
                            ball2 = Ball (model.paddle.x + model.paddle.width / 2) (model.paddle.y - 4) 4 0 -20 False
                            ball3 = Ball (model.paddle.x + model.paddle.width) (model.paddle.y - 4) 4 0 -20 False
                    in
                    ( { model | laserball = [ball1,ball2,ball3], laserfire = False}, Cmd.none)
                    else
                    ( model, Cmd.none )

                Change gameStatus stageStatus->
                    let
                        stage =
                            case stageStatus of
                                Stage1 -> 1
                                Stage2 -> 2
                                Stage3 -> 3
                                Stage4 -> 4
                                Stage5 -> 5
                        initBall =
                            if model.extraballactivation then
                                 initBall2
                            else initBall1
                        fireballtime =
                            if model.fireballactivation then
                                 150
                            else 0
                        laserfire =
                            if model.laseractivation then
                                 True
                            else
                                 False
                    in
                    case gameStatus of
                        Start ->
                            ( { model | ball = initBall
                            , brick = initBricks0
                            , paddle = initPaddle
                            , paddleMove = NotMoving
                            , gameStatus = gameStatus
                            , score = model.score_init
                            , life = model.life_init
                            , stage = stage::model.stage
                            , stageStatus = stageStatus
                            , twisttime = 0
                            , longertime = 0
                            , shortertime = 0
                            , transparenttime = 0
                            , biggertime = 0
                            , fireballtime = fireballtime
                            , laserfire = laserfire
                            , time = 0
                            , messageTime = 0
                            , items = []
                            }
                            , Cmd.none )
                        _ ->
                            ( { model | gameStatus = gameStatus } , Cmd.none )
                Reset gameStatus ->
                    case gameStatus of
                        Start ->
                            ( { model | ball = initBall1
                              , brick = initBricks0
                              , paddle = initPaddle
                              , paddleMove = NotMoving
                              , gameStatus = Start
                              , score = 0
                              , score_init = 0
                              , leftWall = initLeftWall
                              , rightWall = initRightWall
                              , life = 8
                              , life_init = 8
                              , stage = initStage
                              , stageStatus = Stage1
                              , twisttime = 0
                              , longertime = 0
                              , shortertime = 0
                              , transparenttime = 0
                              , biggertime = 0
                              , bonuslist = []
                              , bonusleft = initBonus
                              , fireballactivation = False
                              , fireballtime = 0
                              , extraballactivation = False
                              , laseractivation = False
                              , laserfire = False
                              , laserball = []
                              , time = 0
                              , messageTime = 0
                              , items = []
                              , seed = Random.initialSeed 0
                            }
                            , Cmd.none)
                        _ ->
                            ( { model | gameStatus = gameStatus } , Cmd.none)



        NoButtonPressed playerCommand ->
            case playerCommand of
                MovePaddleLeft ->
                    ( { model | paddleMove = NotMoving }, Cmd.none )

                MovePaddleRight ->
                    ( { model | paddleMove = NotMoving }, Cmd.none )

                LaserFire ->
                    ( { model | paddleMove = NotMoving }, Cmd.none)

                Change _ _ ->
                    ( { model | paddleMove = NotMoving}, Cmd.none )

                Reset _ ->
                    ( { model | paddleMove = NotMoving}, Cmd.none )

        Respawn ->
            ( { model
                | ball = initBall1
                , paddle = initPaddle
                , paddleMove = NotMoving
                , gameStatus = Playing <| Play
                , twisttime = 0
                , longertime = 0
                , shortertime = 0
                , transparenttime = 0
                , biggertime = 0
                , fireballtime = 0
                , messageTime = 0
                , items = []
                , laserball = []
              }
            , Cmd.none
            )

        ChooseBonus bonus ->
            if model.gameStatus == (Playing <| BonusSelection)  then
               let
                  b1 = List.head model.bonuslist
                  b2 = List.head (List.drop 1 model.bonuslist)
                  b3 = List.head (List.drop 2 model.bonuslist)
                  balls =
                      if Just bonus == b2 then
                          initBall2
                      else
                          initBall1
                  fireballtime =
                      if Just bonus == b1 then
                          150
                      else
                          0
                  fireballactivation =
                      if Just bonus == b1 then
                          True
                      else
                          False
                  extraballactivation =
                      if Just bonus == b2 then
                          True
                      else
                          False
                  laseractivation =
                      if Just bonus == b3 then
                          True
                      else
                          False
                  laserfire =
                      if Just bonus == b3 then
                          True
                      else
                          False
               in
                        if checkBonusAvailable bonus model.bonusleft then
                            ( { model
                                  | ball = balls
                                  , paddle = initPaddle
                                  , paddleMove = NotMoving
                                  , gameStatus = (Playing <| NextStage)
                                  , score_init = model.score
                                  , life_init = model.life
                                  , twisttime = 0
                                  , longertime = 0
                                  , shortertime = 0
                                  , transparenttime = 0
                                  , biggertime = 0
                                  , bonusleft = updateBonus bonus model
                                  , fireballtime = fireballtime
                                  , fireballactivation = fireballactivation
                                  , extraballactivation = extraballactivation
                                  , laseractivation = laseractivation
                                  , laserfire = laserfire
                                  , time = 0
                                  , items = []
                                }
                            , Cmd.none
                            )
                        else ( { model | gameStatus = Playing <| ErrorBonus }, Cmd.none)
            else (model,Cmd.none)

        ChooseStage stages ->
            if model.gameStatus == (Playing <| NextStage) || model.gameStatus == (Playing <| Waiting) then
                let
                    initBall =
                        case model.extraballactivation of
                            True ->
                                initBall2
                            False ->
                                initBall1
                in
                case stages of
                Stage1 ->
                    if checkStageAvailable 1 model.stage then
                        ( { model
                              | ball = initBall
                              , paddle = initPaddle
                              , paddleMove = NotMoving
                              , gameStatus = (Playing <| Waiting)
                              , score_init = model.score
                              , life_init = model.life
                              , brick = initBricks1
                              , stage = updateStage 1 model
                              , stageStatus = Stage1
                              , items = []
                            }
                            , Cmd.none
                        )
                    else ( { model | gameStatus = Playing <| Error }, Cmd.none)

                Stage2 ->
                    if checkStageAvailable 2 model.stage then
                        ( { model
                              | ball = initBall
                              , paddle = initPaddle
                              , paddleMove = NotMoving
                              , gameStatus = (Playing <| Waiting)
                              , score_init = model.score
                              , life_init = model.life
                              , brick = initBricks2
                              , stage = updateStage 2 model
                              , stageStatus = Stage2
                            }
                            , Cmd.none
                          )
                    else ( { model | gameStatus = Playing <| Error }, Cmd.none)


                Stage3 ->
                    if checkStageAvailable 3 model.stage then
                        ( { model
                                | ball = initBall
                                , paddle = initPaddle
                                , paddleMove = NotMoving
                                , gameStatus = (Playing <| Waiting)
                                , score_init = model.score
                                , life_init = model.life
                                , brick = initBricks3
                                , stage = updateStage 3 model
                                , stageStatus = Stage3
                              }
                              , Cmd.none
                           )
                    else ( { model | gameStatus = Playing <| Error }, Cmd.none)


                Stage4 ->
                    if checkStageAvailable 4 model.stage then
                    ( { model
                         | ball = initBall
                         , paddle = initPaddle
                         , paddleMove = NotMoving
                         , gameStatus = (Playing <| Waiting)
                         , score_init = model.score
                         , life_init = model.life
                         , brick = initBricks4
                         , stage = updateStage 4 model
                         , stageStatus = Stage4
                         }
                       , Cmd.none
                    )
                    else ( { model | gameStatus = Playing <| Error }, Cmd.none)


                Stage5 ->
                     if checkStageAvailable 5 model.stage then
                    ( { model
                       | ball = initBall
                       , paddle = initPaddle
                       , paddleMove = NotMoving
                       , gameStatus = (Playing <| Waiting)
                       , score_init = model.score
                       , life_init = model.life
                       , brick = initBricks5
                       , stage = updateStage 5 model
                       , stageStatus = Stage5
                       }
                       , Cmd.none
                    )
                    else ( { model | gameStatus = Playing <| Error }, Cmd.none)
            else
                (model,Cmd.none)


--move position from above update to below update
calculatelaser : Ball -> Msg -> Model -> List Ball-> (Model, List Ball)
calculatelaser ball _ model newlist=
    let
        bricks =
            model.brick

        shouldDeleteHorizontalBrick =
            case Tuple.first (Tuple.second (clearBricks_Horizontal ball bricks)) of
                0 -> False
                _ -> True

        shouldDeleteVerticalBrick =
            case Tuple.first (Tuple.second (clearBricks_Vertical ball bricks)) of
                0 -> False
                _ -> True

        ---Detect whether the side that is hit by the ball

        shouldDeleteCornerBrick =
            case Tuple.first (Tuple.second (clearBricks_Corner ball bricks)) of
                0 -> False
                _ -> True
        ----------------------------------
          --partition the function to separated functions so it could be use with displaying message as well
        twistActivation =
            twist model ball 2

        longerActivation =
            longer model ball 2

        shorterActivation =
            shorter model ball 2

        maybeExtraLife =
            extraLife model ball
        -----------------------------------
        newBricks_1 =
            if shouldDeleteHorizontalBrick then
                Tuple.first (clearBricks_Horizontal ball bricks)
            else
                bricks

        newBricks_2 =
            if shouldDeleteVerticalBrick then
                Tuple.first (clearBricks_Vertical ball newBricks_1)
            else
                newBricks_1

        updateBricks =
            if shouldDeleteCornerBrick then
                Tuple.first (clearBricks_Corner ball newBricks_2)
            else
                newBricks_2

        ----------------------------------
        updateball = updatelaserballmove ball

        lostBall =
            if ball.y <= 2 || shouldDeleteHorizontalBrick || shouldDeleteVerticalBrick || shouldDeleteCornerBrick then
                True
            else
                False

        life =
            model.life

        score =
            model.score

        updateScores =
            if shouldDeleteHorizontalBrick || shouldDeleteVerticalBrick || shouldDeleteCornerBrick then
                updateScore model
            else
                0
        --
        --time =
        --    if model.time >= 600 then
        --        0
        --    else
        --        model.time + 1
        --
        twisttime =
            if twistActivation then
                300
            else
                model.twisttime

        longertime =
            if longerActivation then
                600
            else
                model.longertime

        shortertime =
            if shorterActivation then
                600
            else
                model.shortertime

        messageTime =
            if twistActivation || longerActivation || shorterActivation ||  maybeExtraLife == 1
            then 50
            else model.messageTime

        list =
            if not lostBall then
                List.append newlist [updateball]
            else
                newlist

    in
    ( { model
        | brick = updateBricks
        , twisttime = twisttime
        , longertime = longertime
        , shortertime = shortertime
        , score = score + updateScores
        , life = life + maybeExtraLife
        , messageTime = messageTime
      }
    , list
    )


calculate : Ball -> Msg -> Model -> List Ball-> (Model, List Ball)
calculate ball _ model newlist=
    let
        bricks =
            model.brick

        shouldDeleteHorizontalBrick =
            case Tuple.first (Tuple.second (clearBricks_Horizontal ball bricks)) of
                0 -> False
                _ -> True

        shouldDeleteVerticalBrick =
            case Tuple.first (Tuple.second (clearBricks_Vertical ball bricks)) of
                0 -> False
                _ -> True

        ---Detect whether the side that is hit by the ball

        shouldDeleteCornerBrick =
            case Tuple.first (Tuple.second (clearBricks_Corner ball bricks)) of
                0 -> False
                _ -> True

        shouldDeleteItem =
            case Tuple.first (Tuple.second (clearItems ball model.items)) of
                0 -> False
                _ -> True
        ----------------------------------
          --partition the function to separated functions so it could be use with displaying message as well
        twistActivation =
            twist model ball 1

        longerActivation =
            longer model ball 1

        shorterActivation =
            shorter model ball 1

        biggerActivation =
            bigger model ball

        transparentActivation =
            trans model ball


        maybeExtraLife =
            extraLife model ball
        -----------------------------------

        newBricks_1 =
            if shouldDeleteHorizontalBrick then
                Tuple.first (clearBricks_Horizontal ball bricks)
            else
                bricks

        newBricks_2 =
            if shouldDeleteVerticalBrick then
                Tuple.first (clearBricks_Vertical ball newBricks_1)
            else
                newBricks_1

        updateBricks =
            if shouldDeleteCornerBrick then
                Tuple.first (clearBricks_Corner ball newBricks_2)
            else
                newBricks_2

        clearedItems =
            if shouldDeleteItem then
                Tuple.first (clearItems ball updateItems)
            else
                updateItems
        ----------------------------------
        updateBall =
            updateBallMove ball model shouldDeleteHorizontalBrick shouldDeleteVerticalBrick shouldDeleteCornerBrick

        lostBall =
            if ball.y >= model.paddle.y then
                True
            else
                False

        life =
            model.life

        score =
            model.score

        updateScores =
            if shouldDeleteHorizontalBrick || shouldDeleteVerticalBrick || shouldDeleteCornerBrick then
                updateScore model
            else
                0

        ( updateItems , updateSeed ) =
            if model.time == 600 && List.length model.items < 2 then
                let
                    ( newitem , newseed ) = random model.seed
                in
                    (newitem :: model.items , newseed)
            else
                ( model.items , model.seed )
        --
        --time =
        --    if model.time >= 600 then
        --        0
        --    else
        --        model.time + 1
        --
        twisttime =
            if twistActivation then
                300
            else
                model.twisttime

        longertime =
            if longerActivation then
                600
            else
                model.longertime

        shortertime =
            if shorterActivation then
                600
            else
                model.shortertime

        transparenttime =
            if transparentActivation then
                600
            else
                model.transparenttime

        biggertime =
            if biggerActivation then
                600
            else
                model.biggertime


        messageTime =
            if twistActivation || longerActivation || shorterActivation || transparentActivation || biggerActivation || maybeExtraLife == 1
            then 50
            else model.messageTime

        list =
            if not lostBall then
                List.append newlist [updateBall]
            else
                newlist

    in
    ( { model
        | brick = updateBricks
        , items = clearedItems
        , seed = updateSeed
        , twisttime = twisttime
        , longertime = longertime
        , shortertime = shortertime
        , transparenttime = transparenttime
        , biggertime = biggertime
        , score = score + updateScores
        , life = life + maybeExtraLife
        , messageTime = messageTime
      }
    , list
    )

--Partition from calculate to be able to use in message too
twist : Model -> Ball -> Int -> Bool
twist model ball num =
      if (Tuple.second (Tuple.second (clearBricks_Horizontal ball model.brick))).twist == True
                || (Tuple.second (Tuple.second (clearBricks_Vertical ball model.brick))).twist == True
                || (Tuple.second (Tuple.second (clearBricks_Corner ball model.brick))).twist == True
                || Tuple.second (Tuple.second (clearItems ball model.items)) == 2 && num == 1 then
                True
                else
                False

longer : Model -> Ball -> Int -> Bool
longer model ball num =
    if (Tuple.second (Tuple.second (clearBricks_Horizontal ball model.brick))).long == True
                || (Tuple.second (Tuple.second (clearBricks_Vertical ball model.brick))).long == True
                || (Tuple.second (Tuple.second (clearBricks_Corner ball model.brick))).long == True
                || Tuple.second (Tuple.second (clearItems ball model.items)) == 1 && num == 1 then
                True
                else
                False

shorter : Model -> Ball -> Int -> Bool
shorter model ball num =
     if (Tuple.second (Tuple.second (clearBricks_Horizontal ball model.brick))).short == True
                || (Tuple.second (Tuple.second (clearBricks_Vertical ball model.brick))).short == True
                || (Tuple.second (Tuple.second (clearBricks_Corner ball model.brick))).short == True
                || Tuple.second (Tuple.second (clearItems ball model.items)) == 0 && num == 1 then
                True
                else
                False

bigger : Model -> Ball -> Bool
bigger model ball =
    if Tuple.second (Tuple.second (clearItems ball model.items)) == 3 then
                True
                else
                False

trans : Model -> Ball -> Bool
trans model ball =
    if Tuple.second (Tuple.second (clearItems ball model.items)) == 4 then
                True
                else
                False

extraLife : Model -> Ball -> Int
extraLife model ball =
    if (Tuple.second (Tuple.second (clearBricks_Horizontal ball model.brick))).extralife == True
                || (Tuple.second (Tuple.second (clearBricks_Vertical ball model.brick))).extralife == True
                || (Tuple.second (Tuple.second (clearBricks_Corner ball model.brick))).extralife == True then 1
                else 0


calculateUpdate : List Ball -> Msg -> Model -> List Ball -> Model
calculateUpdate balls msg model newlist=
    case balls of
        [] ->
            {model
            | ball = newlist
            }
        ball :: rest ->
            let
                (newmodel, newballlist) = calculate ball msg model newlist
                newestmodel = calculateUpdate rest msg newmodel newballlist
            in
                newestmodel

calculateLaserUpdate : List Ball -> Msg -> Model -> List Ball -> Model
calculateLaserUpdate balls msg model newlist=
    case balls of
        [] ->
            {model
            | laserball = newlist
            }
        ball :: rest ->
            let
                (newmodel, newballlist) = calculatelaser ball msg model newlist
                newestmodel = calculateLaserUpdate rest msg newmodel newballlist
            in
                newestmodel

random : Random.Seed -> ( Item, Random.Seed )
random seed0 =
    let
        ran = Random.weighted (25, 0) [(25, 1), (10, 2), (25, 3), (15, 4)]
        (x, seed1) = Random.step (Random.int 100 900) seed0
        (y, seed2) = Random.step (Random.int 350 450) seed1
        (effect, seed3) = Random.step ran seed2
    in
        ( Item (toFloat x) (toFloat y) effect, seed3 )


updateBallMove : Ball -> Model -> Bool -> Bool -> Bool -> Ball
updateBallMove ball model horizontal_hit vertical_hit corner_hit =
    let
        radius =
            if model.biggertime > 0 then
                16
            else
                8
        transparent =
            if model.transparenttime > 0 then
                True
            else
                False

        detectBouncePaddle =
            bouncePaddle model.paddle ball

        detectBounceWall =
            bounceWall ball

        detectBounceCeiling =
            bounceCeiling ball

        --detect the position of the ball, if the ball reach some specific place it should bounce, gives True/False

        verticalSpeed =
            if detectBouncePaddle then
                ball.verticalSpeed * -1

            else if detectBounceCeiling then
                ball.verticalSpeed * -1

            else if horizontal_hit || corner_hit then
                if model.fireballtime >= 1 && not detectBouncePaddle then
                    ball.verticalSpeed * 1.03
                else
                    if (ball.verticalSpeed <= -7) || (ball.verticalSpeed >= 7) then
                        ball.verticalSpeed * -1
                    else if ball.verticalSpeed < 0 then
                        (ball.verticalSpeed - 0.2) * -1
                    else
                        (ball.verticalSpeed + 0.2) * -1
            else
                ball.verticalSpeed

        --if the ball should bounce (vertical) then reverse verticalSpeed
        horizontalSpeed =
            if detectBounceWall then
                ball.horizontalSpeed * -1

            else if detectBouncePaddle then
                ball.horizontalSpeed / 2 + (ball.verticalSpeed - 1) * (ball.x - model.paddle.x - model.paddle.width / 2) / model.paddle.width * 2

            else if vertical_hit || corner_hit then
                if model.fireballtime >= 1 then
                    ball.horizontalSpeed * 1.03
                else
                    if ball.horizontalSpeed < 0 then
                        (ball.horizontalSpeed - 0.2) * -1
                    else
                        (ball.horizontalSpeed + 0.2) * -1
            else
                ball.horizontalSpeed

        --if the ball should bounce (horizontal) then reverse horizontalSpeed
    in
    { ball | x = ball.x + horizontalSpeed
    , y = ball.y + verticalSpeed
    , horizontalSpeed = horizontalSpeed
    , verticalSpeed = verticalSpeed
    , transparent = transparent
    , r = radius}

updatelaserballmove : Ball -> Ball
updatelaserballmove ball =
    { ball | x = ball.x + ball.horizontalSpeed
    , y = ball.y + ball.verticalSpeed}


updatePaddleMove : Int -> PaddleMove -> Paddle -> Int -> Int -> Paddle
updatePaddleMove twisttime move paddle ltime stime =
    let
        distance =
            case move of
                MoveLeft ->
                    if twisttime > 0 then
                        10
                    else
                        -10

                MoveRight ->
                    if twisttime > 0 then
                        -10
                    else
                        10

                NotMoving ->
                    0

        --if was told to "MoveLeft" or "MoveRight then move left/right by 10 pixel or stay

        width =
            if ltime >= stime && stime > 0 then
                160
            else if stime > ltime then
                80
            else if ltime > 0 then
                160
            else if stime > 0 then
                80
            else
                120

    in
    { paddle
        | x =
            paddle.x
                + distance
                -- update the position of the paddle left or right by distance(15 pixel)
                |> clamp 54 (946 - paddle.width)
        , width = width

        --Clamp the paddle within the screen range
    }


updateCommandAndStatus : Msg -> Model -> List Ball -> (GameStatus, Cmd Msg)
updateCommandAndStatus _ model ball =
    case maybeDead ball of
         Just Dead ->
              let
                  respawn :a-> Msg --always return whatever it gets to message
                  respawn =
                      always Respawn

                  --always return the same type it gets
                  delayRespawn =
                      Process.sleep 1000
                           |> Task.perform respawn

                   -- if life is not 0 then died and respawn
                  gamestatus =
                      if not (loseGame model) then
                            Died
                      else
                            Lose

              in
              ( gamestatus, delayRespawn)

         Nothing ->
              let
                  gamestatus =
                      if (nextStage model) && not (winGame model) && List.length model.stage == List.length model.bonusleft then
                            Playing <| NextStage
                      else if (nextStage model) && not (winGame model) && List.length model.stage < List.length model.bonusleft then
                            Playing <| BonusSelection
                      else if not (loseGame model) && not (winGame model)  then
                            Playing <| Play
                      else if (loseGame model) then
                            Lose
                      else
                            Win

                  command =
                    Cmd.none
              in
              ( gamestatus, command )

updateScore : Model -> Int
updateScore model =
    let
       bricksLeft =
          List.length model.brick

       scoreFloat =
           110 - (toFloat bricksLeft / 0.5) + (toFloat model.life * 2)

       score =
          round scoreFloat
    in
    score

updateLife : Model -> List Ball -> Int
updateLife model ball=
    case maybeDead ball of
        Just Dead ->
            let
                newLife = model.life - 1
            in
            newLife
        Nothing ->
            model.life

--update the list of the stage
updateStage: Int -> Model -> List Int
updateStage number model=
    removePlayedStage number model.stage

updateBonus: Int -> Model -> List Int
updateBonus number model=
    removeSelectedBonus number model.bonusleft

bouncePaddle : Paddle -> Ball -> Bool
bouncePaddle paddle ball =
    (ball.y + ball.r >= paddle.y) && (ball.x >= paddle.x - 5) && (ball.x <= paddle.x + paddle.width + 5) && (ball.verticalSpeed > 0)

--The ball should bounce IFF
-- ball should bounce if the y-coordinate of the ball plus its radius is more than the y coordinate of the paddle
-- and the ball x-coordinate is between x-coordinate of both tip of the paddle
--bouncePaddle gives True or False


bounceWall : Ball -> Bool
bounceWall ball =
    ball.x <= (54 + ball.r) || ball.x >= (946 - ball.r)

--if x-coordinate of the ball exceed screen's x-coordinate then send True, it should bounce
--'screen' here refers to valid game board

bounceCeiling : Ball -> Bool
bounceCeiling ball =
    ball.y <= (49 + ball.r)



--if y-coordinate of the ball exceed the valid game board ceiling height (49) then reverse the direction

detectBrickHorizontalCollision : Ball -> List Brick -> Int -> Maybe Int
detectBrickHorizontalCollision ball bricks num=
    case bricks of
        [] ->
            Nothing
        cell :: rest ->
            let
               brick_x =
                   Tuple.first cell.position
               brick_y =
                   Tuple.second cell.position
               status =
                   (ball.y >= brick_y - ball.r) && (ball.y <= brick_y + ball.r + 30) && (ball.x >= brick_x) && (ball.x <= brick_x + 80)
               status1 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x < brick_x) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed < 0)
               status2 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x > brick_x + 80) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed > 0)
               status3 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x < brick_x) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed < 0)
               status4 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x > brick_x + 80) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed > 0)
            in
            if status || status1 || status2 || status3 || status4 then
               Just num
            else
               detectBrickHorizontalCollision ball rest (num+1)

detectBrickVerticalCollision : Ball -> List Brick -> Int -> Maybe Int
detectBrickVerticalCollision ball bricks num=
    case bricks of
        [] ->
            Nothing
        cell :: rest ->
            let
               brick_x =
                   Tuple.first cell.position
               brick_y =
                   Tuple.second cell.position
               status =
                   (ball.x >= brick_x - ball.r) && (ball.x <= brick_x + ball.r + 80) && (ball.y >= brick_y) && (ball.y <= brick_y + 30)
               status1 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x < brick_x) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed > 0)
               status2 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x > brick_x + 80) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed < 0)
               status3 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x < brick_x) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed < 0)
               status4 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x > brick_x + 80) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed > 0)
            in
            if status || status1 || status2 || status3 || status4 then
               Just num
            else
               detectBrickVerticalCollision ball rest (num+1)

detectBrickCornerCollision : Ball -> List Brick -> Int -> Maybe Int
detectBrickCornerCollision ball bricks num=
    case bricks of
        [] ->
            Nothing
        cell :: rest ->
            let
               brick_x =
                   Tuple.first cell.position
               brick_y =
                   Tuple.second cell.position
               status1 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x < brick_x) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed > 0)
               status2 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x > brick_x + 80) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed < 0)
               status3 =
                   ((ball.y - brick_y - 30)^2 + (ball.x - brick_x)^2 <= (ball.r)^2) && (ball.y > brick_y + 30) && (ball.x < brick_x) && (ball.verticalSpeed < 0) && (ball.horizontalSpeed > 0)
               status4 =
                   ((ball.y - brick_y)^2 + (ball.x - brick_x - 80)^2 <= (ball.r)^2) && (ball.y < brick_y) && (ball.x > brick_x + 80) && (ball.verticalSpeed > 0) && (ball.horizontalSpeed < 0)
            in
            if status1 || status2 || status3 || status4 then
               Just num
            else
               detectBrickCornerCollision ball rest (num+1)

---Detect whether the ball hits the length,the width and the corner of the brick separately

detectItemCollision : Ball -> List Item -> Int -> Maybe Int
detectItemCollision ball items num =
    case items of
        [] ->
            Nothing
        item :: rest ->
            if (item.x + 10 - ball.x) ^ 2 + (item.y + 10 - ball.y) ^ 2 <= (ball.r + 14) ^ 2 then
                Just num
            else
                detectItemCollision ball rest (num + 1)

clearBricks_Horizontal : Ball -> List Brick -> (List Brick, (Int, Brickfunction))
clearBricks_Horizontal ball bricks =
    case detectBrickHorizontalCollision ball bricks 0 of
        Nothing ->
            (bricks, (0, { twist = False , long = False , short = False , extralife = False }))
        Just num ->
            let
                before = List.take num bricks
                after = List.drop (num+1) bricks
                self = List.drop num bricks
                    |> List.head
                (newbricks,(droppedbricks,twistbricks)) = clearBricks_Horizontal ball (before ++ after)

            in
                case self of
                    Just brick ->
                        case brick.status of
                            0 -> (newbricks,(droppedbricks + 1,twistbricks))
                            7 -> (newbricks,(droppedbricks + 1,{twistbricks | extralife = True}))
                            6 -> (newbricks,(droppedbricks + 1,{twistbricks | short = True}))
                            5 -> (newbricks,(droppedbricks + 1,{twistbricks | long = True}))
                            4 -> (newbricks,(droppedbricks + 1,{twistbricks | twist = True}))
                            _ ->
                                let
                                    newbrick = {position = brick.position , status = brick.status - 1}
                                in
                                    (newbrick :: newbricks,(droppedbricks + 1, twistbricks))

                    Nothing -> (newbricks,(droppedbricks , twistbricks))


clearBricks_Vertical : Ball -> List Brick -> (List Brick, (Int , Brickfunction))
clearBricks_Vertical ball bricks =
    case detectBrickVerticalCollision ball bricks 0 of
        Nothing ->
            (bricks, (0, { twist = False , long = False , short = False , extralife = False }))
        Just num ->
            let
                before = List.take num bricks
                after = List.drop (num+1) bricks
                self = List.drop num bricks
                    |> List.head
                (newbricks,(droppedbricks,twistbricks)) = clearBricks_Vertical ball (before ++ after)

            in
                case self of
                    Just brick ->
                        case brick.status of
                            0 -> (newbricks,(droppedbricks + 1,twistbricks))
                            7 -> (newbricks,(droppedbricks + 1,{twistbricks | extralife = True}))
                            6 -> (newbricks,(droppedbricks + 1,{twistbricks | short = True}))
                            5 -> (newbricks,(droppedbricks + 1,{twistbricks | long = True}))
                            4 -> (newbricks,(droppedbricks + 1,{twistbricks | twist = True}))
                            _ ->
                                let
                                    newbrick = {position = brick.position , status = brick.status - 1}
                                in
                                    (newbrick :: newbricks,(droppedbricks + 1, twistbricks))

                    Nothing -> (newbricks,(droppedbricks , twistbricks))

clearBricks_Corner : Ball -> List Brick -> (List Brick, (Int , Brickfunction))
clearBricks_Corner ball bricks =
    case detectBrickCornerCollision ball bricks 0 of
        Nothing ->
            (bricks, (0, { twist = False , long = False , short = False , extralife = False }))
        Just num ->
            let
                before = List.take num bricks
                after = List.drop (num+1) bricks
                self = List.drop num bricks
                    |> List.head
                (newbricks,(droppedbricks,twistbricks)) = clearBricks_Corner ball (before ++ after)

            in
                case self of
                    Just brick ->
                        case brick.status of
                            0 -> (newbricks,(droppedbricks + 1,twistbricks))
                            7 -> (newbricks,(droppedbricks + 1,{twistbricks | extralife = True}))
                            6 -> (newbricks,(droppedbricks + 1,{twistbricks | short = True}))
                            5 -> (newbricks,(droppedbricks + 1,{twistbricks | long = True}))
                            4 -> (newbricks,(droppedbricks + 1,{twistbricks | twist = True}))
                            _ ->
                                let
                                    newbrick = {position = brick.position , status = brick.status - 1}
                                in
                                    (newbrick :: newbricks,(droppedbricks + 1, twistbricks))

                    Nothing -> (newbricks,(droppedbricks , twistbricks))

---Clear the bricks hit by the ball and return the number of blocks hit

clearItems : Ball -> List Item -> (List Item, (Int , Int))
clearItems ball items =
    case detectItemCollision ball items 0 of
        Nothing ->
            (items, (0, -1))
        Just num ->
            let
                before = List.take num items
                after = List.drop (num+1) items
                self = List.drop num items
                    |> List.head
                (newitems,(useditems,itemeffect)) = clearItems ball (before ++ after)
            in
                case self of
                    Just item ->
                        (newitems, (useditems + 1, item.effect))
                    Nothing -> (newitems,(useditems,itemeffect))




--remove recently played stage from the list
removePlayedStage : a -> List a -> List a
removePlayedStage num stage =
    case stage of
        [] ->
            []

        i :: newStage ->
            if num == i then
                newStage

            else
                i :: removePlayedStage num newStage

--remove selected bonus
removeSelectedBonus : a -> List a -> List a
removeSelectedBonus num bonus =
    case bonus of
        [] ->
            []

        i :: newBonus ->
            if num == i then
                newBonus
            else
                i :: removeSelectedBonus num newBonus
--check if there's that stage in the list to play or not
checkStageAvailable : Int -> List Int  -> Bool
checkStageAvailable number stage =
    List.member (number) stage

checkBonusAvailable : Int -> List Int -> Bool
checkBonusAvailable number bonus =
    List.member (number) bonus

nextStage : Model -> Bool
nextStage model =
     List.isEmpty model.brick && model.gameStatus == (Playing <|Play)


maybeDead : List Ball -> Maybe Life
maybeDead ball =
    if List.length ball == 0 then
        Just Dead

    else
        Nothing


loseGame : Model -> Bool
loseGame model=
    model.life == 0


winGame : Model -> Bool
winGame model=
    List.length model.stage == 2 && nextStage model


----------------------------------------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameStatus of
        Playing status ->
            Sub.batch
                [ if status == Play then
                    Browser.Events.onAnimationFrameDelta Animation
                  else
                    Sub.none
                , Browser.Events.onKeyDown (Decode.map ButtonPressed buttonDecoder)
                , Browser.Events.onKeyUp (Decode.map NoButtonPressed buttonDecoder)
                ]

        _ ->
            Sub.none


buttonDecoder : Decode.Decoder PlayerCommand
buttonDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen buttonToPlayerCommand


buttonToPlayerCommand : String -> Decode.Decoder PlayerCommand
buttonToPlayerCommand button =
    case button of
        "ArrowLeft" ->
            Decode.succeed MovePaddleLeft

        "ArrowRight" ->
            Decode.succeed MovePaddleRight

        "ArrowUp" ->
            Decode.succeed LaserFire

        --make decode success and assign MovePaddleLeft/Right to the KeyDown to relay message
        _ ->
            Decode.fail "Other button pressed"


