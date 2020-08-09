module Model exposing (..)

{--
Basic model of the whole program
--}
type alias Model =
    { paddle : Paddle
    , bricks : Bricks
    , ball : Ball
    , state : State
    , moveLeft : Bool
    , moveRight : Bool
    , accelerate : Bool
    , shoot : Bool
    , direction : AnimationState
    , hint : String
    , background1 : Background
    , background2 : Background
    , size : ( Float , Float )
    , passedTime1 : Float
    , passedTime2 : Float
    , passedTime3 : Float
    , moveSteps : Float
    , boss : Boss
    , bossHeight : Float
    }

{--
Used to locate the boss
--}

bossH : Float
bossH =
    11

{--
define the size of the boss
--}
backGround : (Float,Float)
backGround =
    let
        w=1400
        h=700
    in
        (w,h)

{--
All the state included in the game
--}
type State
    = Stopped
    | ClearBricks
    | WinBricks
    | FightBoss
    | FinalWin
    | Lose

{--
Used to detect whether a button is been pressed
--}

type alias AnimationState =
    Maybe
        { active : Bool
        , elapsed : Float
        }

{--
Type for the background
--}
type alias Background =
    { src : String
    , pos : (Float,Float)
    , vy : Float
    }

{--
Used to generate background
--}
genBackground : String -> (Float,Float) -> Float -> Background
genBackground src pos vy=
    Background src pos vy



{--
Type for brick
--}
type alias Brick =
    { coord : (Float,Float)
    , pos : (Float,Float)
    , len : Float
    , wid : Float
    , hp : Int
    , normal : String
    , broken : String
    , direction : Rotate
    }
{--
Type for the rotate direction for the bricks
--}
type Rotate
    = Clock
    | Count
    | Nonet

type alias Bricks =
    List (Brick)

{--
Used to translate (x,y) pos to the real sizes, such as pxs.
--}
calPos : (Float,Float) -> (Float,Float)
calPos (x,y) =
    let
        left= -500
        disX=200
        disY=51
        down= -30
    in
        ((left + disX * x), (down + disY * y))

{--
Used to init bricks
--}
initBrick : (Float,Float,Rotate) ->Brick
initBrick (x,y,r)=
    let

        normal=

                    "./static/brick1.png"

        broken=
                if r == Clock then
                    "./static/broken_brick1.png"

                else if r == Count then
                    "./static/brick2.png"
                else
                    "./static/brick3.png"
        hp=
            if r == Clock then
                2
            else
                1

        pos = calPos (x,y)
    in
    Brick (x,y) pos 75 25 hp normal broken r

{--
This is where all the bricks are generated
--}
initBricks :Bricks
initBricks =
    let
        h=bossH
    in
    List.map initBrick ([(0,h+6,Nonet),(1,h+6,Nonet),(2,h+6,Nonet),(3,h+6,Nonet),(4,h+6,Nonet),(5,h+6,Nonet)
                       , (0,h+5,Nonet),                                                        (5,h+5,Nonet)
                       , (0,h+4,Nonet),                                                        (5,h+4,Nonet)
                       , (0,h+3,Nonet),                                                        (5,h+3,Nonet)
                       , (0,h+2,Nonet),                                                        (5,h+2,Nonet)
                       , (0,h+1,Nonet),                                                        (5,h+1,Nonet)
                       , (0,h+0,Nonet),(1,h+0,Nonet),(2,h+0,Nonet),(3,h+0,Nonet),(4,h+0,Nonet),(5,h+0,Nonet)
                       ]
                                    ++
                       [ (0,10,Nonet),(1,10,Nonet),(2,10,Nonet)
                       ,                                     (3,9,Nonet),(4,9,Nonet),(5,9,Nonet)
                       , (0,8,Nonet),            (2,8,Nonet)            ,(4,8,Nonet)
                       ,             (1,7,Nonet),            (3,7,Nonet),            (5,7,Nonet)
                       ,                         (2,6,Nonet)            ,(4,6,Nonet)
                       ,             (1,5,Nonet),            (3,5,Nonet)
                       , (0,4,Nonet)            ,(2,4,Nonet)            ,(4,4,Nonet)
                       ]
                       )

{--
Type for paddle.
--}
type alias Paddle =
    { src : String
    , pos : (Float,Float)
    , len : Float
    , wid : Float
    }

{--
Used to init paddle
--}
initPaddle : (Float,Float) -> Paddle
initPaddle (x,y) =
    Paddle "./static/Paddle.png"(x,y) 200 60

{--
Type for ball
--}
type alias Ball =
    { pos : (Float,Float)
    , radius : Float
    , velocity : (Float,Float)
    , src : String
    }

{--
Used to init the ball
--}
initBall : (Float,Float) -> Ball
initBall (x,y)=
    Ball (x,y) 15 (-2,2) "./static/ball.png"

{--
Type for the boss
--}
type alias Boss =
    { pos : (Float,Float)
    , size : Float
    , hp : Float
    , src : String
    , velocity : (Float,Float)
    , passedTime : Float
    , is_hurt : Bool
    }

initBoss : String->Boss
initBoss src=
    let
        pos=calPos(2.5,bossH+3)
        size=100
        hp=100
        v=(0,-0.05)
    in
        Boss pos size hp src v 0 False

{--
Used to change the boss's velocity.
When all the bricks move down, the boss stops moving down.
--}
changeVelocity : Boss -> Boss
changeVelocity boss=
    { boss | velocity = (0.5,0),pos=calPos(2.5,3) }

{--
Used to init the whole model
--}
initial : Model
initial =
    let
        p=initPaddle (0,-250)
        bricks=initBricks
        ball=initBall (-200,-250)

        (w,h)=(1920,700)
    in
        Model p bricks  ball
        Stopped False False False False Nothing
        "Press enter to start"
        (genBackground "static/background_01.png" (0,0) -0.5)
        (genBackground "static/background_02.png" (0,700) -0.5)
        (w,h)
        0 0 0 0
        (initBoss "./static/boss_sleep.png")
        bossH

