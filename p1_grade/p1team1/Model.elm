module Model exposing (Model, initialModel, loadStage, State(..), ShoppingStatus(..))

import Entity exposing (Block, Paddle, BlockType(..), Circle, CircleType(..))
import Color
import Random exposing (Seed, initialSeed)
import Message exposing (Buff(..))

type State
    = FirstPage
    | Playing
    | Paused
    | Stopped
    | Manual State
    | Reference State
    | Store State
    | Again

type ShoppingStatus
    = Fine
    | Poor
    | Buy Buff
    | Twice

type alias Model =
    { size : (Float, Float)
    , blocks: List Block
    , paddle: Paddle
    , balls: List Circle
    , width: Int
    , height: Int
    , status: State
    , score: Int
    , coins: List Circle
    , bonusBalls : List Circle
    , seed : Seed
    , twoPlayerMode : Bool
    , longPaddle : Bool
    , fasterPaddle : Bool
    , moreScore : Bool
    , life : Int
    , shopping : ShoppingStatus
    , initialBalls : List Circle
    , initialPaddle : Paddle
    , hasStarted : Bool
    , stage : Int
    , requiredScore : Int
    }

-- Some config constants of the game
gameWidth : Int
gameWidth = 1183


gameHeight : Int
gameHeight = 650




setBlockPos : (Float, Float) -> Block
setBlockPos (x,y) =
       { category = SimpleBlock
        , color = Color.fromName "red"
        , rect =
            { width = 90
            , height = 45
            , centerPos = ( x , y )
            }
        , isDestroyed = False
        }

setSolidBlockPos : (Float, Float) -> Block
setSolidBlockPos (x,y) =
       { category = SolidBlock
        , color = Color.fromName "red"
        , rect =
            { width = 90
            , height = 45
            , centerPos = ( x , y )
            }
        , isDestroyed = False
        }

setStrongBlockPos : (Float, Float) -> Block
setStrongBlockPos (x,y) =
       { category = StrongBlock 2
        , color = Color.fromName "red"
        , rect =
            { width = 90
            , height = 45
            , centerPos = ( x , y )
            }
        , isDestroyed = False
        }

setTreasureBlockPos : (Float, Float) -> Block
setTreasureBlockPos (x,y) =
       { category = TreasureBlock 15
        , color = Color.fromName "red"
        , rect =
            { width = 90
            , height = 45
            , centerPos = ( x , y )
            }
        , isDestroyed = False
        }

setBallBlockPos : (Float, Float) -> Block
setBallBlockPos (x,y) =
       { category = BallBlock
        , color = Color.fromName "red"
        , rect =
            { width = 90
            , height = 45
            , centerPos = ( x , y )
            }
        , isDestroyed = False
        }

blockFromList : List (Float, Float) -> List Block
blockFromList pos =
    List.map setBlockPos pos

initialPaddle : Paddle
initialPaddle =
    { rect =
        { width = 200
        , height = 36
        , centerPos = ( toFloat gameWidth / 2 , toFloat gameHeight / 6 * 5)
        }
    , color = Color.fromName "blue"
    }

initialBlocks : List Block
initialBlocks =
    [setSolidBlockPos (45,22.5), setBlockPos (136,22.5), setBallBlockPos (227, 22.5), setBlockPos (318, 22.5), setBlockPos (409, 22.5), setBlockPos (500, 22.5),setBlockPos(591,22.5),setBlockPos(682,22.5),setTreasureBlockPos(773,22.5),setBlockPos(864,22.5),setBlockPos(955,22.5),setBlockPos(1046,22.5),setBlockPos(1137,22.5)
    ,setBlockPos (45,71.5), setBlockPos (136,71.5), setTreasureBlockPos (227, 71.5), setBlockPos (318, 71.5), setBlockPos (409, 71.5), setTreasureBlockPos (500, 71.5),setBlockPos(591,71.5),setBlockPos(682,71.5),setBlockPos(773,71.5),setBlockPos(864,71.5),setBlockPos(955,71.5),setSolidBlockPos(1046,71.5),setBlockPos(1137,71.5)
    ,setBlockPos (45,120.5), setStrongBlockPos (136,120.5), setBlockPos (227, 120.5), setBlockPos (318, 120.5), setBlockPos (409, 120.5), setBlockPos (500, 120.5),setBlockPos(591,120.5),setBlockPos(682,120.5),setBlockPos(773,120.5),setBlockPos(864,120.5),setTreasureBlockPos(955,120.5),setBlockPos(1046,120.5),setBlockPos(1137,120.5)
    ,setTreasureBlockPos (45,169.5), setBlockPos (136,169.5), setBlockPos (227, 169.5), setTreasureBlockPos (318, 169.5), setBlockPos (409, 169.5), setBlockPos (500, 169.5),setBlockPos(591,169.5),setBlockPos(682,169.5),setStrongBlockPos(773,169.5),setBlockPos(864,169.5),setBlockPos(955,169.5),setBlockPos(1046,169.5),setTreasureBlockPos(1137,169.5)
    ,setStrongBlockPos (45,218.5), setTreasureBlockPos (136,218.5), setBlockPos (227, 218.5), setBallBlockPos (318, 218.5), setSolidBlockPos (409, 218.5), setTreasureBlockPos (500, 218.5),setBlockPos(591,218.5),setTreasureBlockPos(682,218.5),setBlockPos(773,218.5),setBlockPos(864,218.5),setStrongBlockPos(955,218.5),setBlockPos(1046,218.5),setBlockPos(1137,218.5)
  --  ,setBlockPos (45,267.5), setBlockPos (136,267.5), setBlockPos (227, 267.5), setBlockPos (318, 267.5), setBlockPos (409, 267.5), setBlockPos (500, 267.5),setBlockPos(591,267.5),setBlockPos(682,267.5),setBlockPos(773,267.5),setBlockPos(864,267.5),setBlockPos(955,267.5),setBlockPos(1046,267.5),setBlockPos(1137,267.5)
    ]

loadStage : Int -> Model -> Model
loadStage stage model =
    let
        x =
            case stage of
                2 ->
                    [setBlockPos (45,22.5), setStrongBlockPos (136,22.5), setBlockPos (227, 22.5), setSolidBlockPos (318, 22.5), setBlockPos (409, 22.5), setBlockPos (500, 22.5),setBlockPos(591,22.5),setStrongBlockPos(682,22.5),setBlockPos(773,22.5),setBlockPos(864,22.5),setTreasureBlockPos(955,22.5),setBlockPos(1046,22.5),setBlockPos(1137,22.5)
                    ,setBlockPos (45,71.5), setBlockPos (136,71.5), setBlockPos (227, 71.5), setBlockPos (318, 71.5), setTreasureBlockPos (409, 71.5), setBlockPos (500, 71.5),setTreasureBlockPos(591,71.5),setBlockPos(682,71.5),setBlockPos(773,71.5),setBlockPos(864,71.5),setStrongBlockPos(955,71.5),setBlockPos(1046,71.5),setBlockPos(1137,71.5)
                    ,setBlockPos (45,120.5), setTreasureBlockPos (136,120.5), setBlockPos (227, 120.5), setStrongBlockPos (318, 120.5), setBlockPos (409, 120.5), setBlockPos (500, 120.5),setBlockPos(591,120.5),setBlockPos(682,120.5),setTreasureBlockPos(773,120.5),setBlockPos(864,120.5),setStrongBlockPos(955,120.5),setBlockPos(1046,120.5),setBlockPos(1137,120.5)
                    ,setTreasureBlockPos (45,169.5), setStrongBlockPos (136,169.5), setBallBlockPos (227, 169.5), setBlockPos (318, 169.5), setTreasureBlockPos (409, 169.5), setBlockPos (500, 169.5),setBlockPos(591,169.5),setSolidBlockPos(682,169.5),setStrongBlockPos(773,169.5),setBlockPos(864,169.5),setBlockPos(955,169.5),setBlockPos(1046,169.5),setTreasureBlockPos(1137,169.5)
                    ,setStrongBlockPos (45,218.5), setTreasureBlockPos (136,218.5), setBlockPos (227, 218.5), setSolidBlockPos (318, 218.5), setBlockPos (409, 218.5), setStrongBlockPos (500, 218.5),setBlockPos(591,218.5),setStrongBlockPos(682,218.5),setTreasureBlockPos(773,218.5),setBlockPos(864,218.5),setBlockPos(955,218.5),setStrongBlockPos(1046,218.5),setBlockPos(1137,218.5) ]
                3 ->
                    [setSolidBlockPos (45,22.5), setTreasureBlockPos (136,22.5), setBlockPos (227, 22.5), setBlockPos (318, 22.5), setBlockPos (409, 22.5), setSolidBlockPos (500, 22.5),setBlockPos(591,22.5),setStrongBlockPos(682,22.5),setBlockPos(773,22.5),setBlockPos(864,22.5),setStrongBlockPos(955,22.5),setBlockPos(1046,22.5),setBlockPos(1137,22.5)
                    ,setBlockPos (45,71.5), setBlockPos (136,71.5), setStrongBlockPos (227, 71.5), setBlockPos (318, 71.5), setBlockPos (409, 71.5), setBlockPos (500, 71.5),setStrongBlockPos(591,71.5),setTreasureBlockPos(682,71.5),setBlockPos(773,71.5),setStrongBlockPos(864,71.5),setBlockPos(955,71.5),setTreasureBlockPos(1046,71.5),setBlockPos(1137,71.5)
                    ,setBlockPos (45,120.5), setTreasureBlockPos (136,120.5), setBlockPos (227, 120.5), setBlockPos (318, 120.5), setTreasureBlockPos (409, 120.5), setBlockPos (500, 120.5),setBallBlockPos(591,120.5),setBlockPos(682,120.5),setSolidBlockPos(773,120.5),setTreasureBlockPos(864,120.5),setBlockPos(955,120.5),setBlockPos(1046,120.5),setBlockPos(1137,120.5)
                    ,setBlockPos (45,169.5), setStrongBlockPos (136,169.5), setTreasureBlockPos (227, 169.5), setBlockPos (318, 169.5), setBlockPos (409, 169.5), setStrongBlockPos (500, 169.5),setTreasureBlockPos(591,169.5),setBlockPos(682,169.5),setBlockPos(773,169.5),setBlockPos(864,169.5),setBlockPos(955,169.5),setStrongBlockPos(1046,169.5),setBlockPos(1137,169.5)
                    ,setSolidBlockPos (45,218.5), setBlockPos (136,218.5), setStrongBlockPos (227, 218.5), setBlockPos (318, 218.5), setTreasureBlockPos (409, 218.5), setSolidBlockPos (500, 218.5),setBlockPos(591,218.5),setBlockPos(682,218.5),setStrongBlockPos(773,218.5),setBlockPos(864,218.5),setBlockPos(955,218.5),setStrongBlockPos(1046,218.5),setTreasureBlockPos(1137,218.5) ]
                4 ->
                    [setTreasureBlockPos (45,22.5), setStrongBlockPos (136,22.5), setBlockPos (227, 22.5), setBlockPos (318, 22.5), setTreasureBlockPos (409, 22.5), setBlockPos (500, 22.5),setSolidBlockPos(591,22.5),setBlockPos(682,22.5),setBlockPos(773,22.5),setTreasureBlockPos(864,22.5),setTreasureBlockPos(955,22.5),setBlockPos(1046,22.5),setBlockPos(1137,22.5)
                    ,setStrongBlockPos (45,71.5), setBlockPos (136,71.5), setStrongBlockPos (227, 71.5), setBallBlockPos (318, 71.5), setBlockPos (409, 71.5), setBlockPos (500, 71.5),setBlockPos(591,71.5),setBlockPos(682,71.5),setBlockPos(773,71.5),setBlockPos(864,71.5),setBlockPos(955,71.5),setBlockPos(1046,71.5),setBlockPos(1137,71.5)
                    ,setBlockPos (45,120.5), setTreasureBlockPos (136,120.5), setBlockPos (227, 120.5), setSolidBlockPos (318, 120.5), setBlockPos (409, 120.5), setTreasureBlockPos (500, 120.5),setBlockPos(591,120.5),setBlockPos(682,120.5),setBlockPos(773,120.5),setTreasureBlockPos(864,120.5),setBlockPos(955,120.5),setStrongBlockPos(1046,120.5),setSolidBlockPos(1137,120.5)
                    ,setTreasureBlockPos (45,169.5), setStrongBlockPos (136,169.5), setBlockPos (227, 169.5), setSolidBlockPos (318, 169.5), setBlockPos (409, 169.5), setStrongBlockPos (500, 169.5),setSolidBlockPos(591,169.5),setBlockPos(682,169.5),setStrongBlockPos(773,169.5),setStrongBlockPos(864,169.5),setBlockPos(955,169.5),setSolidBlockPos(1046,169.5),setTreasureBlockPos(1137,169.5)
                    ,setStrongBlockPos (45,218.5), setBlockPos (136,218.5), setSolidBlockPos (227, 218.5), setBlockPos (318, 218.5), setStrongBlockPos (409, 218.5), setBlockPos (500, 218.5),setBlockPos(591,218.5),setStrongBlockPos(682,218.5),setTreasureBlockPos(773,218.5),setBlockPos(864,218.5),setBlockPos(955,218.5),setBlockPos(1046,218.5),setStrongBlockPos(1137,218.5) ]
                _ ->
                    initialBlocks
    in
        {model| blocks = x, balls = [initialBall1], coins = [], bonusBalls = [], paddle = initialPaddle}


initialBall1 : Circle
initialBall1 =
    { pos = ( toFloat gameWidth / 2 , toFloat gameHeight / 10 * 6)
    , radius = 15
    , color = Color.fromName "green"
    , velocityX = -45
    , velocityY = -225
    , category = Ball}

initialModel: Model
initialModel =
    { size = (0,0)
    , blocks = initialBlocks
    , paddle = initialPaddle
    , balls = [initialBall1]
    , width = gameWidth
    , height = gameHeight
    , status = FirstPage
    , score = 0
    , coins = []
    , bonusBalls = []
    , seed = initialSeed 8615648
    , twoPlayerMode = False
    , life = 3
    , longPaddle = False
    , fasterPaddle = False
    , moreScore = False
    , shopping = Fine
    , initialBalls = [initialBall1]
    , initialPaddle = initialPaddle
    , hasStarted = False
    , stage = 1
    , requiredScore = 100
    }


