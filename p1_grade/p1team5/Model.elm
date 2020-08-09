module Model exposing (..)

import Array
import Browser.Dom exposing (getViewport)
import List exposing (concat)
import Message exposing (Msg(..))
import Random
import Task



--model definition


type alias Model =
    { paddle : Object
    , ball : List Object
    , brick : List Object
    , bonus : List Bonus
    , state : State
    , winsize : ( Float, Float )
    , gametime : Float

    --generating random number function
    , random : Int
    , score : Int

    --the flag of the second stage
    , secondflag : Int
    , secondtime : Float
    , ballspeed : Float
    }


type State
    = Paused
    | Playing
    | Stopped
    | Win
    | Lose
    | Choose
    | Instr


type alias Object =
    { center_pos : ( Float, Float )
    , object_type : Object_type
    , size : ( Float, Float )
    , direction : Maybe Float
    , hp : Maybe Int
    , brick_type : Maybe Brick_type ---------------*
    , point : Maybe Int
    , id : Int
    , bonusTime : Float
    }


type alias Bonus =
    { xPos : Float
    , yPos : Float
    , height : Float
    , bonusType : Int
    , state : Int
    , id : Int
    }



--state 1 -> untouched, 2 -> dropping, 3 -> deleted


type Brick_type
    = --------------*
      Good
    | Guard
    | Evil
    | Controlled


type Object_type
    = Brick
    | Paddle
    | Ball


generateBonus bricks bonusType id =
    let
        accordingBrick =
            Array.fromList bricks
                |> Array.get id
                |> Maybe.withDefault (Object ( 0, 0 ) Brick ( 0.0, 0.0 ) Nothing Nothing Nothing Nothing 1 0)

        ( a, b ) =
            accordingBrick.center_pos

        ( x, y ) =
            accordingBrick.size
    in
    Bonus (a + x / 2)
        (b + y / 2)
        (y / 2)
        bonusType
        1
        id



--to generate the list of bricks for initialmodel


generateBricks : Model -> Int -> Int -> Int -> List Object
generateBricks model numpercolomn numperrow evilpos =
    let
        width =
            Tuple.first model.winsize / toFloat numperrow

        height =
            Tuple.second model.winsize / 3.0 / toFloat numpercolomn

        width1 =
            0.4 * width

        height1 =
            0.8 * height

        createGoodBrick k =
            ----------*
            Object
                ( toFloat (remainderBy numperrow k) * width, toFloat (k // numperrow) * height )
                Brick
                ( width1, height1 )
                Nothing
                (Just 1)
                (Just Good)
                (Just -20)
                k
                0

        createGuard k =
            --------------*
            Object
                ( toFloat (remainderBy numperrow k) * width, toFloat (k // numperrow) * height )
                Brick
                ( width1, height1 )
                Nothing
                (Just 2)
                (Just Guard)
                (Just 0)
                k
                0

        createEvil k =
            -----------------------*
            Object
                ( toFloat (remainderBy numperrow k) * width, toFloat (k // numperrow) * height )
                Brick
                ( width1, height1 )
                Nothing
                (Just 5)
                (Just Evil)
                (Just 0)
                k
                0
    in
    ---------------------------*
    if remainderBy numperrow evilpos == 0 then
        concat
            [ List.map createGoodBrick (List.range 0 (evilpos - 9))
            , List.map createGuard (List.range (evilpos - 8) (evilpos - 7))
            , List.map createGoodBrick (List.range (evilpos - 6) (evilpos - 1))
            , [ createEvil evilpos ]
            , [ createGuard (evilpos + 1) ]
            , List.map createGoodBrick (List.range (evilpos + 2) (evilpos + 7))
            , List.map createGuard (List.range (evilpos + 8) (evilpos + 9))
            , List.map createGoodBrick (List.range (evilpos + 9) (numpercolomn * numperrow - 1))
            ]

    else if remainderBy numperrow evilpos == 7 then
        concat
            [ List.map createGoodBrick (List.range 0 (evilpos - 10))
            , List.map createGuard (List.range (evilpos - 9) (evilpos - 8))
            , List.map createGoodBrick (List.range (evilpos - 7) (evilpos - 2))
            , [ createGuard (evilpos - 1) ]
            , [ createEvil evilpos ]
            , List.map createGoodBrick (List.range (evilpos + 1) (evilpos + 6))
            , List.map createGuard (List.range (evilpos + 7) (evilpos + 8))
            , List.map createGoodBrick (List.range (evilpos + 8) (numpercolomn * numperrow - 1))
            ]

    else
        concat
            [ List.map createGoodBrick (List.range 0 (evilpos - 10))
            , List.map createGuard (List.range (evilpos - 9) (evilpos - 7))
            , List.map createGoodBrick (List.range (evilpos - 6) (evilpos - 2))
            , [ createGuard (evilpos - 1) ]
            , [ createEvil evilpos ]
            , [ createGuard (evilpos + 1) ]
            , List.map createGoodBrick (List.range (evilpos + 2) (evilpos + 6))
            , List.map createGuard (List.range (evilpos + 7) (evilpos + 9))
            , List.map createGoodBrick (List.range (evilpos + 10) (numpercolomn * numperrow - 1))
            ]



--to generate the ball by the given radius


initBall : Float -> Object
initBall radius =
    Object ( 0, 0 ) Ball ( radius, radius ) (Just 1.5) Nothing Nothing Nothing 1 0



--to generate the paddle


initPaddle : Float -> Float -> Object
initPaddle width height =
    Object ( 0, 0 ) Paddle ( width, height ) Nothing Nothing Nothing Nothing 1 0



--init the model


initialmodel : Model
initialmodel =
    { paddle = initPaddle 300.0 10.0
    , ball = [ initBall 30.0 ]
    , brick = []
    , bonus = []
    , state = Stopped
    , winsize = ( 500, 500 )
    , gametime = 0
    , random = 20
    , score = 300
    , secondflag = 0
    , secondtime = 0
    , ballspeed = 5
    }



--init function
--further we can use getviewport to optimize the window


numberGenerator : Random.Generator Int
numberGenerator =
    Random.uniform 9
        [ 10
        , 11
        , 12
        , 13
        , 14
        , 17
        , 18
        , 19
        , 20
        , 21
        , 22
        , 25
        , 26
        , 27
        , 28
        , 29
        , 30
        , 33
        , 34
        , 35
        , 36
        , 37
        , 38
        ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialmodel, Cmd.batch [ Task.perform GetViewport getViewport, Random.generate Newnum numberGenerator ] )
